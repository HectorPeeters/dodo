use crate::ast::UpperStatement;
use crate::project::{
    Project, BUILTIN_TYPE_U16, BUILTIN_TYPE_U32, BUILTIN_TYPE_U64, BUILTIN_TYPE_U8,
    BUILTIN_TYPE_VOID,
};
use crate::tokenizer::SourceRange;
use crate::types::Type;
use crate::{
    ast::{BinaryOperatorType, ConsumingAstVisitor, Expression, Statement, UnaryOperatorType},
    backend::Backend,
    error::{Error, ErrorType, Result},
    scope::Scope,
    types::TypeId,
    x86_instruction::{X86Instruction, X86Operand, X86Register, RAX, RBP, RDX, RSP},
};
use std::{fs::File, io::Write, path::Path, process::Command};
use X86Instruction::*;
use X86Operand::*;
use X86Register::*;

const ARGUMENT_REGISTERS: [X86Register; 6] = [Rdi, Rsi, Rdx, Rcx, R8, R9];
const GENERAL_PURPOSE_REGISTER_COUNT: usize = 6;
const GENERAL_PURPOSE_REGISTER_OFFSET: usize = 10;
const STACK_OFFSET: usize = 16;

#[derive(Debug, Copy, Clone)]
pub enum ScopeLocation {
    Stack(usize),
    Global(usize),
}

pub enum LValueLocation {
    Stack(usize),
    Register(X86Register, usize),
}

impl LValueLocation {
    pub fn offset(self, offset: usize) -> LValueLocation {
        match self {
            LValueLocation::Stack(o) => LValueLocation::Stack(o + offset),
            LValueLocation::Register(r, o) => LValueLocation::Register(r, o + offset),
        }
    }
}

struct GlobalConst {
    index: usize,
    value: Expression,
    value_type: TypeId,
    annotations: Vec<(String, Option<Expression>)>,
}

pub struct X86NasmGenerator<'a> {
    project: &'a mut Project,
    instructions: Vec<X86Instruction>,
    label_index: usize,
    scope: Scope<ScopeLocation>,
    allocated_registers: [bool; GENERAL_PURPOSE_REGISTER_COUNT],
    current_function_end_label: usize,
    global_consts: Vec<GlobalConst>,
}

impl<'a> X86NasmGenerator<'a> {
    pub fn new(project: &'a mut Project) -> Self {
        let mut result = Self {
            project,
            instructions: vec![],
            label_index: 0,
            scope: Scope::new(),
            allocated_registers: [false; GENERAL_PURPOSE_REGISTER_COUNT],
            current_function_end_label: 0,
            global_consts: vec![],
        };

        result.instr(Section(".text".to_string()));
        result.instr(Global("_start".to_string()));

        result
    }

    fn get_next_register(&mut self) -> X86Register {
        match self.allocated_registers.iter().position(|x| !x) {
            Some(reg) => {
                self.allocated_registers[reg] = true;
                X86Register::from(reg + GENERAL_PURPOSE_REGISTER_OFFSET)
            }
            None => unreachable!(),
        }
    }

    fn free_register(&mut self, reg: X86Register) {
        assert!(self.allocated_registers[reg as usize - GENERAL_PURPOSE_REGISTER_OFFSET]);
        self.allocated_registers[reg as usize - GENERAL_PURPOSE_REGISTER_OFFSET] = false;
    }

    fn get_new_label(&mut self) -> usize {
        let result = self.label_index;
        self.label_index += 1;
        result
    }

    fn store_global_const(
        &mut self,
        value: Expression,
        value_type: TypeId,
        annotations: Vec<(String, Option<Expression>)>,
    ) -> usize {
        let index = self.get_new_label();
        self.global_consts.push(GlobalConst {
            index,
            value,
            value_type,
            annotations,
        });
        index
    }

    fn instr(&mut self, instr: X86Instruction) {
        self.instructions.push(instr);
    }

    fn write_prologue(&mut self) {
        self.instr(Push(RBP));
        self.instr(Push(RBP));
        self.instr(Mov(RBP, RSP));
    }

    fn write_epilogue(&mut self) {
        self.instr(Mov(RSP, RBP));
        self.instr(Pop(RBP));
        self.instr(Pop(RBP));
    }

    fn get_data_instructions(&self, expression: &Expression, size: usize) -> Vec<X86Instruction> {
        let mut instructions = vec![];

        match expression {
            Expression::IntegerLiteral(value, _, _) => {
                use X86Instruction::*;

                let instruction = match size {
                    8 => Db(vec![*value as u8]),
                    16 => Dw(vec![*value as u16]),
                    32 => Dd(vec![*value as u32]),
                    64 => Dq(vec![*value]),
                    _ => unreachable!(),
                };

                instructions.push(instruction);
            }
            Expression::StringLiteral(value, _, _) => {
                let mut string_bytes = value.as_bytes().to_vec();
                string_bytes.push(0);
                instructions.push(Db(string_bytes));
            }
            Expression::BooleanLiteral(value, _, _) => {
                instructions.push(Db(vec![if *value { 1 } else { 0 }]));
            }
            Expression::StructLiteral(fields, _, _) => {
                for field in fields {
                    let mut field_instructions = self.get_data_instructions(
                        &field.1,
                        self.project.get_type_size(field.1.get_type()),
                    );

                    instructions.append(&mut field_instructions);
                }
            }
            Expression::Widen(expr, widen_type, _) => {
                let mut child_instructions =
                    self.get_data_instructions(expr, self.project.get_type_size(*widen_type));

                instructions.append(&mut child_instructions);
            }
            _ => unreachable!(),
        }

        instructions
    }

    fn write_data<T: Write>(&mut self, writer: &mut T) {
        let mut queued_instructions = vec![];

        for global in &self.global_consts {
            let section_annotation = global.annotations
                    .iter()
                    .find(|(name, value)|  matches!(value, Some(Expression::StringLiteral(..)) if name == "section" ))
                    .map(|(_, value)| match value {
                        Some(Expression::StringLiteral(value, _, _)) => value,
                        _ => unreachable!()
                    });

            if let Some(section_name) = section_annotation {
                queued_instructions.push(Section(section_name.to_string()));
            } else {
                queued_instructions.push(Section(".data".to_string()));
            }

            queued_instructions.push(LabelDef(global.index));

            let mut data_instructions = self.get_data_instructions(
                &global.value,
                self.project.get_type_size(global.value.get_type()),
            );

            queued_instructions.append(&mut data_instructions);
        }

        for instr in queued_instructions {
            self.instr(instr);
        }

        writeln!(
            writer,
            "{}",
            self.instructions
                .iter()
                .map(|instr| format!("{}{}", if instr.should_indent() { "\t" } else { "" }, instr))
                .collect::<Vec<_>>()
                .join("\n")
        )
        .unwrap();
    }

    fn get_struct_field_offset(&self, name: &str, struct_type: TypeId) -> usize {
        let struct_type = self.project.get_struct(struct_type);

        let mut offset = 0;
        for field in &struct_type.fields {
            if field.0 == name {
                break;
            }
            offset += self.project.get_type_size(field.1);
        }

        offset
    }

    fn generate_lvalue(
        &mut self,
        expression: Expression,
        range: SourceRange,
    ) -> Result<(LValueLocation, Option<X86Register>)> {
        match expression {
            Expression::VariableRef(name, _, _) => {
                let location = self.scope.find(&name).map_err(|x| x.with_range(range))?;

                match location {
                    ScopeLocation::Stack(offset) => {
                        Ok((LValueLocation::Stack(offset * STACK_OFFSET), None))
                    }
                    ScopeLocation::Global(_) => todo!(),
                }
            }
            Expression::UnaryOperator(UnaryOperatorType::Deref, expr, _value_type, _) => {
                let dest_reg = self.visit_expression(*expr)?;

                Ok((LValueLocation::Register(dest_reg, 0), Some(dest_reg)))
            }
            Expression::FieldAccessor(name, child, _, _) => {
                let offset = self.get_struct_field_offset(&name, child.get_type());

                let (child_location, child_reg) = self.generate_lvalue(*child, range)?;

                Ok((child_location.offset(offset), child_reg))
            }
            _ => todo!("generate_lvalue not implemented for {:?}", expression),
        }
    }
}

impl<'a> Backend for X86NasmGenerator<'a> {
    fn process_upper_statement(&mut self, statement: UpperStatement) -> Result<()> {
        self.visit_upper_statement(statement)
    }

    fn finalize(&mut self, output: &Path, dont_compile: bool) -> Result<()> {
        // Generating assembly

        let assembly_file = output.with_extension("asm");
        let mut assembly_output = File::create(&assembly_file).unwrap();
        self.write_data(&mut assembly_output);

        if dont_compile {
            return Ok(());
        }

        // Compiling assemlby

        let object_file = std::env::temp_dir().join(format!("{:?}.o", output.file_name().unwrap()));
        let assembler_output = Command::new("nasm")
            .args([
                "-f",
                "elf64",
                assembly_file.to_str().unwrap(),
                "-o",
                object_file.to_str().unwrap(),
                "-g",
            ])
            .output()
            .expect("Failed to execute assembler");

        assembler_output.status.exit_ok().map_err(|_| {
            Error::new(
                ErrorType::Postprocess,
                format!(
                    "Assembling failed\n\n{}",
                    String::from_utf8(assembler_output.stderr).unwrap()
                ),
            )
        })?;

        // Linking

        let linker_output = Command::new("gcc")
            .args([
                "-o",
                output.to_str().unwrap(),
                object_file.to_str().unwrap(),
                "-nostartfiles",
                "-no-pie",
                "-g",
            ])
            .output()
            .expect("Failed to execute linker");

        linker_output.status.exit_ok().map_err(|_| {
            Error::new(
                ErrorType::Postprocess,
                format!(
                    "Linking failed\n\n{}",
                    String::from_utf8(linker_output.stderr).unwrap()
                ),
            )
        })?;

        Ok(())
    }

    fn name(&self) -> &'static str {
        "nasm-x86_64"
    }
}

impl<'a> ConsumingAstVisitor<(), (), X86Register> for X86NasmGenerator<'a> {
    fn visit_upper_statement(&mut self, statement: UpperStatement) -> Result<()> {
        match statement {
            UpperStatement::StructDeclaratin(_, _) => {}
            UpperStatement::ExternDeclaration(symbol, _) => {
                self.instr(Extern(symbol));
            }
            UpperStatement::Function(name, args, _ret_type, body, annotations, range) => {
                self.scope.push();

                assert!(args.len() <= 6);
                assert_eq!(self.allocated_registers.iter().filter(|x| **x).count(), 0);

                assert_eq!(self.current_function_end_label, 0);
                self.current_function_end_label = self.get_new_label();

                let no_return = annotations.iter().any(|(name, _)| name == "noreturn");

                let section_annotation = annotations
                    .into_iter()
                    .find(|(name, value)|  matches!(value, Some(Expression::StringLiteral(..)) if name == "section" ))
                    .map(|(_, value)| match value {
                        Some(Expression::StringLiteral(value, _, _)) => value,
                        _ => unreachable!()
                    });

                if let Some(section_name) = section_annotation {
                    self.instr(Section(section_name));
                }

                self.instr(Function(if name == "main" {
                    "_start".to_string()
                } else {
                    name
                }));

                self.write_prologue();

                if !args.is_empty() {
                    self.instr(Sub(RSP, Constant((STACK_OFFSET * args.len()) as u64)));
                }

                for ((arg_name, arg_type), arg_reg) in args.iter().zip(ARGUMENT_REGISTERS) {
                    let offset = self.scope.size()?;

                    self.scope
                        .insert(arg_name, ScopeLocation::Stack(offset))
                        .map_err(|x| x.with_range(range))?;

                    self.instr(Mov(
                        RegIndirect(Rbp, offset * STACK_OFFSET),
                        Reg(arg_reg, self.project.get_type_size(*arg_type)),
                    ));
                }

                self.visit_statement(*body)?;

                if no_return {
                    return Ok(());
                }

                self.instr(LabelDef(self.current_function_end_label));
                self.current_function_end_label = 0;

                self.write_epilogue();

                self.instr(Ret());
                self.scope.pop();

                assert_eq!(self.allocated_registers.iter().filter(|x| **x).count(), 0);
            }
            UpperStatement::ConstDeclaration(name, value_type, value, annotations, _range) => {
                let position = self.store_global_const(value, value_type, annotations);
                self.scope.insert(&name, ScopeLocation::Global(position))?;
            }
        }

        Ok(())
    }

    fn visit_statement(&mut self, statement: Statement) -> Result<()> {
        match statement {
            Statement::Declaration(name, _value_type, range) => {
                self.scope
                    .insert(&name, ScopeLocation::Stack(self.scope.size()?))
                    .map_err(|x| x.with_range(range))?;

                self.instr(Sub(RSP, Constant(STACK_OFFSET as u64)));
            }
            Statement::Assignment(lhs, rhs, range) => {
                let value_size = self.project.get_type_size(rhs.get_type());
                let value_reg = self.visit_expression(rhs)?;
                let value_operand = Reg(value_reg, value_size);

                let (lvalue_operand, reg_to_free) = self.generate_lvalue(lhs, range)?;

                match lvalue_operand {
                    LValueLocation::Stack(offset) => {
                        self.instr(Mov(RegIndirect(Rbp, offset), value_operand))
                    }
                    LValueLocation::Register(reg, offset) => {
                        self.instr(Mov(RegIndirect(reg, offset), value_operand))
                    }
                }

                if let Some(reg_to_free) = reg_to_free {
                    self.free_register(reg_to_free);
                }

                self.free_register(value_reg);
            }
            Statement::While(cond, stmt, _range) => {
                let start_label = self.get_new_label();
                let end_label = self.get_new_label();

                self.instr(LabelDef(start_label));

                let cond_size = self.project.get_type_size(cond.get_type());
                let register = self.visit_expression(cond)?;

                self.instr(Cmp(Reg(register, cond_size), Constant(0)));
                self.instr(Jz(Label(end_label)));

                self.visit_statement(*stmt)?;

                self.instr(Jmp(Label(start_label)));
                self.instr(LabelDef(end_label));

                self.free_register(register);
            }
            Statement::If(cond, stmt, else_body, _range) => {
                let end_label = self.get_new_label();
                let else_label = self.get_new_label();

                let cond_size = self.project.get_type_size(cond.get_type());
                let cond_register = self.visit_expression(cond)?;

                self.instr(Cmp(Reg(cond_register, cond_size), Constant(0)));
                self.free_register(cond_register);

                if else_body.is_none() {
                    self.instr(Jz(Label(end_label)));
                } else {
                    self.instr(Jz(Label(else_label)));
                }

                self.visit_statement(*stmt)?;

                if let Some(else_body) = else_body {
                    self.instr(Jmp(Label(end_label)));

                    self.instr(LabelDef(else_label));

                    self.visit_statement(*else_body)?;
                }

                self.instr(LabelDef(end_label));
            }
            Statement::Return(expr, _range) => {
                let expr_size = self.project.get_type_size(expr.get_type());
                let value_reg = self.visit_expression(expr).unwrap();

                self.instr(Mov(Reg(Rax, expr_size), Reg(value_reg, expr_size)));
                self.instr(Jmp(Label(self.current_function_end_label)));

                self.free_register(value_reg);
            }
            Statement::Block(stmts, scoped, _) => {
                if scoped {
                    self.scope.push();
                }

                for stmt in stmts {
                    self.visit_statement(stmt)?;
                }

                if scoped {
                    self.scope.pop();
                }
            }
            Statement::Expression(expr, _range) => {
                let register = self.visit_expression(expr)?;
                self.free_register(register);
            }
        }
        Ok(())
    }

    fn visit_expression(&mut self, expression: Expression) -> Result<X86Register> {
        match expression {
            Expression::IntegerLiteral(value, value_type, _range) => {
                let register = self.get_next_register();

                let value_size = self.project.get_type_size(value_type);
                self.instr(Mov(Reg(register, value_size), Constant(value)));

                Ok(register)
            }
            Expression::BooleanLiteral(value, _, _range) => {
                let register = self.get_next_register();

                self.instr(Mov(Reg(register, 8), Constant(if value { 1 } else { 0 })));

                Ok(register)
            }
            Expression::UnaryOperator(UnaryOperatorType::Ref, expr, ref_type, _range) => {
                let result_reg = self.get_next_register();

                match &*expr {
                    Expression::VariableRef(name, _, range) => {
                        let location = self.scope.find(name).map_err(|x| x.with_range(*range))?;

                        match location {
                            ScopeLocation::Stack(offset) => self.instr(Lea(
                                Reg(result_reg, self.project.get_type_size(ref_type)),
                                RegIndirect(Rbp, offset * 16),
                            )),
                            ScopeLocation::Global(_) => todo!(),
                        }
                        Ok(result_reg)
                    }
                    _ => unreachable!(),
                }
            }
            Expression::UnaryOperator(op, expr, _, _range) => {
                let expr_size = self.project.get_type_size(expr.get_type());

                let reg = self.visit_expression(*expr)?;
                let result_reg = self.get_next_register();

                match op {
                    UnaryOperatorType::Deref => {
                        self.instr(Mov(Reg(result_reg, expr_size), RegIndirect(reg, 0)));
                    }
                    _ => todo!("Cannot generate code for {:?}", op),
                }

                self.free_register(reg);
                Ok(result_reg)
            }
            Expression::BinaryOperator(op, left, right, _, _range) => {
                let left_size = self.project.get_type_size(left.get_type());
                let right_size = self.project.get_type_size(right.get_type());
                let left_reg = self.visit_expression(*left)?;
                let right_reg = self.visit_expression(*right)?;

                assert_eq!(left_size, right_size);

                let left_op = Reg(left_reg, left_size);
                let right_op = Reg(right_reg, right_size);

                match op {
                    BinaryOperatorType::Add => {
                        self.instr(Add(left_op, right_op));
                    }
                    BinaryOperatorType::Subtract => {
                        self.instr(Sub(left_op, right_op));
                    }
                    BinaryOperatorType::Multiply => {
                        assert!(
                            left_size != 8 && right_size != 8,
                            "We don't support multiplication of 8 bit integers"
                        );
                        self.instr(Mul(left_op, right_op));
                    }
                    BinaryOperatorType::Divide => {
                        self.instr(Mov(Reg(Rax, left_size), left_op.clone()));
                        self.instr(Mov(RDX, Constant(0)));
                        self.instr(Div(right_op));
                        self.instr(Mov(left_op, Reg(Rax, left_size)));
                    }
                    BinaryOperatorType::Modulo => {
                        self.instr(Mov(Reg(Rax, left_size), left_op.clone()));
                        self.instr(Mov(RDX, Constant(0)));
                        self.instr(Div(right_op));
                        self.instr(Mov(left_op, Reg(Rdx, left_size)));
                    }
                    BinaryOperatorType::ShiftLeft => {
                        // TODO: this could overwrite RCX
                        self.instr(Mov(Reg(Rcx, right_size), right_op));
                        self.instr(Shl(left_op, Reg(Rcx, 8)));
                    }
                    BinaryOperatorType::ShiftRight => {
                        // TODO: this could overwrite RCX
                        self.instr(Mov(Reg(Rcx, right_size), right_op));
                        self.instr(Shr(left_op, Reg(Rcx, 8)));
                    }
                    BinaryOperatorType::Equal => {
                        self.instr(Cmp(left_op, right_op));
                        self.instr(SetZ(Reg(left_reg, 8)));
                    }
                    BinaryOperatorType::NotEqual => {
                        self.instr(Cmp(left_op, right_op));
                        self.instr(SetNZ(Reg(left_reg, 8)));
                    }
                    BinaryOperatorType::LessThan => {
                        self.instr(Cmp(left_op, right_op));
                        self.instr(SetL(Reg(left_reg, 8)));
                    }
                    BinaryOperatorType::LessThanEqual => {
                        self.instr(Cmp(left_op, right_op));
                        self.instr(SetLE(Reg(left_reg, 8)));
                    }
                    BinaryOperatorType::GreaterThan => {
                        self.instr(Cmp(left_op, right_op));
                        self.instr(SetG(Reg(left_reg, 8)));
                    }
                    BinaryOperatorType::GreaterThanEqual => {
                        self.instr(Cmp(left_op, right_op));
                        self.instr(SetGE(Reg(left_reg, 8)));
                    }
                }

                self.free_register(right_reg);
                Ok(left_reg)
            }
            Expression::VariableRef(name, value_type, range) => {
                let location = self.scope.find(&name).map_err(|x| x.with_range(range))?;
                let register = self.get_next_register();

                let value_size = self.project.get_type_size(value_type);

                match location {
                    ScopeLocation::Stack(offset) => self.instr(Mov(
                        Reg(register, value_size),
                        RegIndirect(Rbp, offset * 16),
                    )),
                    ScopeLocation::Global(index) => {
                        if self.project.is_ptr_type(value_type)
                            || self.project.is_struct_type(value_type)
                        {
                            self.instr(Mov(Reg(register, value_size), Label(index)))
                        } else {
                            self.instr(Mov(Reg(register, value_size), LabelIndirect(index)))
                        }
                    }
                }

                Ok(register)
            }
            Expression::FunctionCall(name, args, return_type, _range) => {
                assert!(args.len() <= 6);

                let arg_count = args.len();

                for (expr, dest_reg) in args.into_iter().zip(&ARGUMENT_REGISTERS) {
                    let arg_size = self.project.get_type_size(expr.get_type());
                    let arg_register = self.visit_expression(expr)?;

                    if dest_reg.is_caller_saved() {
                        self.instr(Push(Reg(*dest_reg, 64)));
                    }
                    self.instr(Mov(Reg(*dest_reg, arg_size), Reg(arg_register, arg_size)));
                    self.free_register(arg_register);
                }

                for reg in 0..GENERAL_PURPOSE_REGISTER_COUNT {
                    if self.allocated_registers[reg] {
                        let reg = X86Register::from(reg + GENERAL_PURPOSE_REGISTER_OFFSET);
                        self.instr(Push(Reg(reg, 64)));
                    }
                }

                // TODO: get rid of this disgusting hack
                if name == "printf" {
                    self.instr(Mov(RAX, Constant(0)));
                }

                self.instr(Call(Reference(name)));

                for reg in (0..GENERAL_PURPOSE_REGISTER_COUNT).rev() {
                    if self.allocated_registers[reg] {
                        let reg = X86Register::from(reg + GENERAL_PURPOSE_REGISTER_OFFSET);
                        self.instr(Pop(Reg(reg, 64)));
                    }
                }

                let result_register = self.get_next_register();

                if return_type != BUILTIN_TYPE_VOID {
                    let return_type_size = self.project.get_type_size(return_type);

                    self.instr(Mov(
                        Reg(result_register, return_type_size),
                        Reg(Rax, return_type_size),
                    ));
                }

                for i in (0..arg_count).rev() {
                    if ARGUMENT_REGISTERS[i].is_caller_saved() {
                        self.instr(Pop(Reg(ARGUMENT_REGISTERS[i], 64)));
                    }
                }

                Ok(result_register)
            }
            Expression::StringLiteral(value, _, range) => {
                let u8_pointer_type = self.project.find_or_add_type(Type::Ptr(BUILTIN_TYPE_U8));

                let label = self.store_global_const(
                    Expression::StringLiteral(value, u8_pointer_type, range),
                    u8_pointer_type,
                    vec![],
                );
                let result_reg = self.get_next_register();
                self.instr(Mov(Reg(result_reg, 64), Label(label)));
                Ok(result_reg)
            }
            Expression::StructLiteral(_, _, _) => todo!(),
            Expression::FieldAccessor(name, child, value_type, _) => {
                let value_type_size = self.project.get_type_size(value_type);
                assert!(value_type_size <= 64);

                let offset = self.get_struct_field_offset(&name, child.get_type());
                let child_reg = self.visit_expression(*child)?;

                let result_reg = self.get_next_register();
                self.instr(Mov(
                    Reg(result_reg, value_type_size),
                    RegIndirect(child_reg, offset),
                ));

                self.free_register(child_reg);

                Ok(result_reg)
            }
            Expression::Widen(expr, widen_type, _range) => {
                let expr_size = self.project.get_type_size(expr.get_type());
                let widen_size = self.project.get_type_size(widen_type);

                assert!(expr_size < widen_size);
                let expr_reg = self.visit_expression(*expr)?;

                let result_reg = self.get_next_register();
                self.instr(MovZX(Reg(result_reg, widen_size), Reg(expr_reg, expr_size)));

                self.free_register(expr_reg);

                Ok(result_reg)
            }
            Expression::Cast(_, _, _) => todo!(),
        }
    }
}
