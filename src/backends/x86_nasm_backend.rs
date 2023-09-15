use super::x86_instruction::{
    X86Instruction, X86Instruction::*, X86Operand::*, X86Register, X86Register::*,
};
use super::x86_instruction::{RAX, RBP, RDX, RSP};
use super::Backend;
use crate::ast::{
    Annotations, AssignmentStatement, Ast, AstVisitor, BlockStatement, ConstDeclaration,
    DeclarationStatement, ExpressionId, ExpressionStatement, FieldAccessorExpr, FunctionCallExpr,
    FunctionDeclaration, IfStatement, IntegerLiteralExpr, ReturnStatement, StatementId,
    UnaryOperatorExpr, UpperStatement, UpperStatementId, VariableRefExpr, WhileStatement,
    WidenExpr,
};
use crate::sema::{DeclarationId, Sema, BUILTIN_TYPE_VOID};
use crate::{
    ast::{BinaryOperatorType, Expression, Statement, UnaryOperatorType},
    error::{Error, ErrorType, Result},
    types::TypeId,
};
use std::collections::HashMap;
use std::ops::AddAssign;
use std::{fs::File, io::Write, path::Path, process::Command};

const ARGUMENT_REGISTERS: [X86Register; 6] = [Rdi, Rsi, Rdx, Rcx, R8, R9];
const GENERAL_PURPOSE_REGISTER_COUNT: usize = 6;
const GENERAL_PURPOSE_REGISTER_OFFSET: usize = 10;
const STACK_OFFSET: usize = 16;

#[derive(Debug, Copy, Clone)]
pub enum DataLocation {
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

struct GlobalConst<'a> {
    index: usize,
    expression: ExpressionId,
    annotations: Annotations<'a>,
}

pub struct X86NasmBackend<'a> {
    ast: &'a Ast<'a>,
    sema: &'a Sema<'a>,
    data_locations: HashMap<DeclarationId, DataLocation>,
    data_stack_offset: Vec<usize>,
    instructions: Vec<X86Instruction>,
    label_index: usize,
    allocated_registers: [bool; GENERAL_PURPOSE_REGISTER_COUNT],
    current_function_end_label: usize,
    global_consts: Vec<GlobalConst<'a>>,
}

impl<'a> X86NasmBackend<'a> {
    pub fn new(ast: &'a Ast, sema: &'a Sema) -> Self {
        let mut result = Self {
            ast,
            sema,
            data_locations: HashMap::new(),
            data_stack_offset: vec![0],
            instructions: vec![],
            label_index: 0,
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
        expression: ExpressionId,
        annotations: Annotations<'a>,
    ) -> usize {
        let index = self.get_new_label();
        self.global_consts.push(GlobalConst {
            index,
            expression,
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

    fn get_data_instructions(
        &self,
        expression_id: ExpressionId,
        size: usize,
    ) -> Result<Vec<X86Instruction>> {
        let mut instructions = vec![];

        let expression = self.ast.get_expression(expression_id);

        match expression {
            Expression::IntegerLiteral(int_lit) => {
                use X86Instruction::*;

                let instruction = match size {
                    8 => Db(vec![int_lit.value as u8]),
                    16 => Dw(vec![int_lit.value as u16]),
                    32 => Dd(vec![int_lit.value as u32]),
                    64 => Dq(vec![int_lit.value]),
                    _ => unreachable!(),
                };

                instructions.push(instruction);
            }
            Expression::StringLiteral(str_lit) => {
                let mut string_bytes = str_lit.value.to_string().as_bytes().to_vec();
                string_bytes.push(0);
                instructions.push(Db(string_bytes));
            }
            Expression::BooleanLiteral(bool_lit) => {
                instructions.push(Db(vec![u8::from(bool_lit.value)]));
            }
            Expression::StructLiteral(struct_lit) => {
                for field in &struct_lit.fields {
                    let field_expression = self.ast.get_expression(field.1);

                    let mut field_instructions = self.get_data_instructions(
                        field.1,
                        self.sema.get_type_size(field_expression.type_id())?,
                    )?;

                    instructions.append(&mut field_instructions);
                }
            }
            Expression::Widen(WidenExpr {
                expr,
                type_id,
                range: _,
            }) => {
                let mut child_instructions =
                    self.get_data_instructions(*expr, self.sema.get_type_size(*type_id)?)?;

                instructions.append(&mut child_instructions);
            }
            _ => unreachable!("{:?}", expression),
        }

        Ok(instructions)
    }

    fn write_data<T: Write>(&mut self, writer: &mut T) -> Result<()> {
        let mut queued_instructions = vec![];

        for global in &self.global_consts {
            let section_annotation = global.annotations.get_string("section", self.ast);

            if let Some(section_name) = section_annotation {
                queued_instructions.push(Section(section_name.to_string()));
            } else {
                queued_instructions.push(Section(".data".to_string()));
            }

            queued_instructions.push(LabelDef(global.index));

            let global_expression = self.ast.get_expression(global.expression);

            let mut data_instructions = self.get_data_instructions(
                global.expression,
                self.sema.get_type_size(global_expression.type_id())?,
            )?;

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

        Ok(())
    }

    fn get_struct_field_offset(&self, name: &str, struct_type: TypeId) -> Result<usize> {
        let struct_type = self.sema.get_struct(struct_type)?;

        let mut offset = 0;
        for field in &struct_type.fields {
            if field.0 == name {
                break;
            }
            offset += self.sema.get_type_size(field.1)?;
        }

        Ok(offset)
    }

    fn generate_lvalue(
        &mut self,
        expression_id: ExpressionId,
    ) -> Result<(LValueLocation, Option<X86Register>)> {
        let expression = self.ast.get_expression(expression_id);

        match expression {
            Expression::VariableRef(var_ref) => {
                let location = self.data_locations.get(&var_ref.declaration_id).unwrap();

                match location {
                    DataLocation::Stack(offset) => {
                        Ok((LValueLocation::Stack(offset * STACK_OFFSET), None))
                    }
                    DataLocation::Global(_) => todo!(),
                }
            }
            Expression::UnaryOperator(UnaryOperatorExpr {
                op_type: UnaryOperatorType::Deref,
                expr,
                type_id: _,
                range: _,
            }) => {
                let dest_reg = self.visit_expression(*expr)?;

                Ok((LValueLocation::Register(dest_reg, 0), Some(dest_reg)))
            }
            Expression::FieldAccessor(FieldAccessorExpr {
                name,
                expr,
                type_id: _,
                range: _,
            }) => {
                let expression = self.ast.get_expression(*expr);

                let offset = self.get_struct_field_offset(name, expression.type_id())?;

                let (child_location, child_reg) = self.generate_lvalue(*expr)?;

                Ok((child_location.offset(offset), child_reg))
            }
            _ => todo!("generate_lvalue not implemented for {:?}", expression),
        }
    }
}

impl<'a> Backend<'a> for X86NasmBackend<'a> {
    fn process(&mut self) -> Result<()> {
        for upper_statement_id in 0..self.ast.upper_statements.len() {
            self.visit_upper_statement((upper_statement_id as u32).into())?;
        }
        Ok(())
    }

    fn finalize(&mut self, output: &Path, dont_compile: bool) -> Result<()> {
        // Generating assembly

        println!("Instruction count: {}", self.instructions.len());

        let assembly_file = output.with_extension("asm");
        let mut assembly_output = File::create(&assembly_file).unwrap();
        self.write_data(&mut assembly_output)?;

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

impl<'a> AstVisitor<'a, (), (), X86Register> for X86NasmBackend<'a> {
    fn visit_upper_statement(&mut self, statement_id: UpperStatementId) -> Result<()> {
        let statement = self.ast.get_upper_statement(statement_id);

        match statement {
            UpperStatement::StructDeclaration(_) => {}
            UpperStatement::ExternDeclaration(extern_decl) => {
                self.instr(Extern(extern_decl.name.to_string()));
            }
            UpperStatement::Function(FunctionDeclaration {
                name,
                params,
                return_type: _,
                body,
                annotations,
                range: _,
            }) => {
                self.data_stack_offset.push(0);

                assert!(params.len() <= 6);
                assert_eq!(self.allocated_registers.iter().filter(|x| **x).count(), 0);

                assert_eq!(self.current_function_end_label, 0);
                self.current_function_end_label = self.get_new_label();

                let no_return = annotations.has_flag("noreturn");
                let section_annotation = annotations.get_string("section", self.ast);

                if let Some(section_name) = section_annotation {
                    self.instr(Section(section_name.to_string()));
                }

                let function_name = if *name == "main" { "_start" } else { name };
                self.instr(Function(function_name.to_string()));

                self.write_prologue();

                if !params.is_empty() {
                    self.instr(Sub(RSP, Constant((STACK_OFFSET * params.len()) as u64)));
                }

                for (declaration_id, arg_reg) in params.iter().zip(ARGUMENT_REGISTERS) {
                    let offset = self.data_stack_offset.iter().sum();

                    self.data_locations
                        .insert(*declaration_id, DataLocation::Stack(offset));
                    self.data_stack_offset.last_mut().unwrap().add_assign(1);

                    let arg_type = self.sema.get_declaration(*declaration_id).type_id;

                    self.instr(Mov(
                        RegIndirect(Rbp, offset * STACK_OFFSET, true),
                        Reg(arg_reg, self.sema.get_type_size(arg_type)?),
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

                self.data_stack_offset.pop().unwrap();

                assert_eq!(self.allocated_registers.iter().filter(|x| **x).count(), 0);
            }
            UpperStatement::ConstDeclaration(ConstDeclaration {
                value,
                annotations,
                declaration_id,
                range: _,
            }) => {
                let position = self.store_global_const(*value, annotations.clone());
                self.data_locations
                    .insert(*declaration_id, DataLocation::Global(position));
                self.data_stack_offset.last_mut().unwrap().add_assign(1);
            }
        }

        Ok(())
    }

    fn visit_statement(&mut self, statement_id: StatementId) -> Result<()> {
        let statement = self.ast.get_statement(statement_id);

        match statement {
            Statement::Declaration(DeclarationStatement {
                declaration_id,
                range: _,
            }) => {
                let offset = self.data_stack_offset.iter().sum();

                self.data_locations
                    .insert(*declaration_id, DataLocation::Stack(offset));
                self.data_stack_offset.last_mut().unwrap().add_assign(1);

                self.instr(Sub(RSP, Constant(STACK_OFFSET as u64)));
            }
            Statement::Assignment(AssignmentStatement {
                left,
                right,
                range: _,
            }) => {
                let right_expr = self.ast.get_expression(*right);

                assert!(!self.sema.get_type(right_expr.type_id())?.is_struct());

                let value_size = self.sema.get_type_size(right_expr.type_id())?;
                let value_reg = self.visit_expression(*right)?;
                let value_operand = Reg(value_reg, value_size);

                let (lvalue_operand, reg_to_free) = self.generate_lvalue(*left)?;

                match lvalue_operand {
                    LValueLocation::Stack(offset) => {
                        self.instr(Mov(RegIndirect(Rbp, offset, true), value_operand))
                    }
                    LValueLocation::Register(reg, offset) => {
                        self.instr(Mov(RegIndirect(reg, offset, true), value_operand))
                    }
                }

                if let Some(reg_to_free) = reg_to_free {
                    self.free_register(reg_to_free);
                }

                self.free_register(value_reg);
            }
            Statement::While(WhileStatement {
                condition,
                body,
                range: _,
            }) => {
                let start_label = self.get_new_label();
                let end_label = self.get_new_label();

                self.instr(LabelDef(start_label));

                let condition_expr = self.ast.get_expression(*condition);
                let condition_size = self.sema.get_type_size(condition_expr.type_id())?;
                let register = self.visit_expression(*condition)?;

                self.instr(Cmp(Reg(register, condition_size), Constant(0)));
                self.instr(Jz(Label(end_label)));

                self.visit_statement(*body)?;

                self.instr(Jmp(Label(start_label)));
                self.instr(LabelDef(end_label));

                self.free_register(register);
            }
            Statement::If(IfStatement {
                condition,
                if_body,
                else_body,
                range: _,
            }) => {
                let end_label = self.get_new_label();
                let else_label = self.get_new_label();

                let condition_expr = self.ast.get_expression(*condition);
                let condition_size = self.sema.get_type_size(condition_expr.type_id())?;
                let condition_register = self.visit_expression(*condition)?;

                self.instr(Cmp(Reg(condition_register, condition_size), Constant(0)));
                self.free_register(condition_register);

                if else_body.is_none() {
                    self.instr(Jz(Label(end_label)));
                } else {
                    self.instr(Jz(Label(else_label)));
                }

                self.visit_statement(*if_body)?;

                if let Some(else_body) = else_body {
                    self.instr(Jmp(Label(end_label)));

                    self.instr(LabelDef(else_label));

                    self.visit_statement(*else_body)?;
                }

                self.instr(LabelDef(end_label));
            }
            Statement::Return(ReturnStatement { expr, range: _ }) => {
                let value = self.ast.get_expression(*expr);
                let expr_size = self.sema.get_type_size(value.type_id())?;
                let value_reg = self.visit_expression(*expr)?;

                self.instr(Mov(Reg(Rax, expr_size), Reg(value_reg, expr_size)));
                self.instr(Jmp(Label(self.current_function_end_label)));

                self.free_register(value_reg);
            }
            Statement::Block(BlockStatement {
                children,
                scoped,
                range: _,
            }) => {
                if *scoped {
                    self.data_stack_offset.push(0);
                }

                for stmt in children {
                    self.visit_statement(*stmt)?;
                }

                if *scoped {
                    self.data_stack_offset.pop();
                }
            }
            Statement::Expression(ExpressionStatement { expr, range: _ }) => {
                let register = self.visit_expression(*expr)?;
                self.free_register(register);
            }
        }
        Ok(())
    }

    fn visit_expression(&mut self, expression_id: ExpressionId) -> Result<X86Register> {
        let expression = self.ast.get_expression(expression_id);

        match expression {
            Expression::IntegerLiteral(IntegerLiteralExpr {
                value,
                type_id,
                range: _,
            }) => {
                let register = self.get_next_register();

                let value_size = self.sema.get_type_size(*type_id)?;
                self.instr(Mov(Reg(register, value_size), Constant(*value)));

                Ok(register)
            }
            Expression::BooleanLiteral(bool_lit) => {
                let register = self.get_next_register();

                self.instr(Mov(Reg(register, 8), Constant(u64::from(bool_lit.value))));

                Ok(register)
            }
            Expression::UnaryOperator(UnaryOperatorExpr {
                op_type: UnaryOperatorType::Ref,
                expr,
                type_id,
                range: _,
            }) => {
                let result_reg = self.get_next_register();

                let expression = self.ast.get_expression(*expr);

                match expression {
                    Expression::VariableRef(VariableRefExpr {
                        name: _,
                        declaration_id,
                        type_id: _,
                        range: _,
                    }) => {
                        let location = self.data_locations[declaration_id];

                        let value_size = self.sema.get_type_size(*type_id)?;

                        match location {
                            DataLocation::Stack(offset) => self.instr(Lea(
                                Reg(result_reg, value_size),
                                RegIndirect(Rbp, offset * 16, true),
                            )),
                            DataLocation::Global(index) => {
                                self.instr(Mov(Reg(result_reg, value_size), Label(index)))
                            }
                        }
                        Ok(result_reg)
                    }
                    _ => unreachable!(),
                }
            }
            Expression::UnaryOperator(unop) => {
                let expr = self.ast.get_expression(unop.expr);
                let expr_size = self.sema.get_type_size(expr.type_id())?;

                let reg = self.visit_expression(unop.expr)?;
                let result_reg = self.get_next_register();

                match unop.op_type {
                    UnaryOperatorType::Deref => {
                        self.instr(Mov(Reg(result_reg, expr_size), RegIndirect(reg, 0, true)));
                    }
                    _ => todo!("Cannot generate code for {:?}", unop.op_type),
                }

                self.free_register(reg);
                Ok(result_reg)
            }
            Expression::BinaryOperator(binop) => {
                let left = self.ast.get_expression(binop.left);
                let right = self.ast.get_expression(binop.right);

                let left_size = self.sema.get_type_size(left.type_id())?;
                let right_size = self.sema.get_type_size(right.type_id())?;

                let left_reg = self.visit_expression(binop.left)?;
                let right_reg = self.visit_expression(binop.right)?;

                assert_eq!(left_size, right_size);

                let left_op = Reg(left_reg, left_size);
                let right_op = Reg(right_reg, right_size);

                match binop.op_type {
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
                    BinaryOperatorType::LogicalOr => self.instr(Or(left_op, right_op)),
                    BinaryOperatorType::LogicalAnd => self.instr(And(left_op, right_op)),
                }

                self.free_register(right_reg);
                Ok(left_reg)
            }
            Expression::VariableRef(VariableRefExpr {
                name: _,
                declaration_id,
                type_id,
                range: _,
            }) => {
                let location = self.data_locations[&declaration_id];
                let register = self.get_next_register();

                let value_size = self.sema.get_type_size(*type_id)?;

                match location {
                    DataLocation::Stack(offset) => self.instr(Mov(
                        Reg(register, value_size),
                        RegIndirect(Rbp, offset * 16, true),
                    )),
                    DataLocation::Global(index) => {
                        let value_type = self.sema.get_type(*type_id)?;

                        if value_type.is_ptr() || value_type.is_struct() {
                            self.instr(Mov(Reg(register, value_size), Label(index)))
                        } else {
                            self.instr(Mov(Reg(register, value_size), LabelIndirect(index)))
                        }
                    }
                }

                Ok(register)
            }
            Expression::FunctionCall(FunctionCallExpr {
                name,
                args,
                type_id,
                range: _,
            }) => {
                assert!(args.len() <= 6);

                let arg_count = args.len();

                for (expr, dest_reg) in args.iter().zip(&ARGUMENT_REGISTERS) {
                    let expression = self.ast.get_expression(*expr);
                    let arg_size = self.sema.get_type_size(expression.type_id())?;
                    let arg_register = self.visit_expression(*expr)?;

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
                if *name == "printf" {
                    self.instr(Mov(RAX, Constant(0)));
                }

                self.instr(Call(Reference(name.to_string())));

                for reg in (0..GENERAL_PURPOSE_REGISTER_COUNT).rev() {
                    if self.allocated_registers[reg] {
                        let reg = X86Register::from(reg + GENERAL_PURPOSE_REGISTER_OFFSET);
                        self.instr(Pop(Reg(reg, 64)));
                    }
                }

                let result_register = self.get_next_register();

                if *type_id != BUILTIN_TYPE_VOID {
                    let return_type_size = self.sema.get_type_size(*type_id)?;

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
            Expression::StringLiteral(_) => {
                let label = self.store_global_const(expression_id, Annotations::empty());
                let result_reg = self.get_next_register();
                self.instr(Mov(Reg(result_reg, 64), Label(label)));
                Ok(result_reg)
            }
            Expression::StructLiteral(_) => todo!(),
            Expression::FieldAccessor(FieldAccessorExpr {
                name,
                expr,
                type_id,
                range: _,
            }) => {
                let value_type_size = self.sema.get_type_size(*type_id)?;
                assert!(value_type_size <= 64);

                let expression = self.ast.get_expression(*expr);
                let offset = self.get_struct_field_offset(name, expression.type_id())? / 8;
                let child_reg = self.visit_expression(*expr)?;

                let result_reg = self.get_next_register();
                self.instr(Mov(
                    Reg(result_reg, value_type_size),
                    RegIndirect(child_reg, offset, false),
                ));

                self.free_register(child_reg);

                Ok(result_reg)
            }
            Expression::Widen(WidenExpr {
                expr,
                type_id,
                range: _,
            }) => {
                let expression = self.ast.get_expression(*expr);
                let expr_size = self.sema.get_type_size(expression.type_id())?;
                let widen_size = self.sema.get_type_size(*type_id)?;

                assert!(expr_size < widen_size);
                let expr_reg = self.visit_expression(*expr)?;

                let result_reg = self.get_next_register();

                // TODO: there should be a better way
                if expr_size == 32 && widen_size == 64 {
                    self.instr(Mov(Reg(result_reg, widen_size), Constant(0)));
                    self.instr(Mov(Reg(result_reg, expr_size), Reg(expr_reg, expr_size)));
                } else {
                    self.instr(MovZX(Reg(result_reg, widen_size), Reg(expr_reg, expr_size)));
                }

                self.free_register(expr_reg);

                Ok(result_reg)
            }
            Expression::Cast(cast) => self.visit_expression(cast.expr),
            Expression::Type(_) => unreachable!(),
        }
    }
}
