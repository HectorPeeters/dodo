use std::io::Write;

use crate::{
    ast::{BinaryOperatorType, Expression, Statement, UnaryOperatorType},
    error::Result,
    scope::Scope,
    types::Type,
    x86_instruction::{X86Instruction, X86Operand, X86Register, RAX, RBP, RSP},
};

use X86Instruction::*;
use X86Operand::*;
use X86Register::*;

const ARGUMENT_REGISTERS: [X86Register; 6] = [Rdi, Rsi, Rdx, Rcx, R8, R9];
const GENERAL_PURPOSE_REGISTER_OFFSET: usize = 10;
const STACK_OFFSET: usize = 16;

pub type ScopeLocation = (usize, Type);

pub struct X86NasmGenerator<'a> {
    instructions: Vec<X86Instruction>,
    label_index: usize,
    scope: Scope<'a, ScopeLocation>,
    allocated_registers: [bool; GENERAL_PURPOSE_REGISTER_OFFSET],
    strings: Vec<&'a str>,
}

impl<'a> X86NasmGenerator<'a> {
    pub fn new(source_file: &'a str) -> Self {
        Self {
            instructions: vec![],
            label_index: 0,
            scope: Scope::new(source_file),
            allocated_registers: [false; GENERAL_PURPOSE_REGISTER_OFFSET],
            strings: vec![],
        }
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

    fn store_new_string(&mut self, string: &'a str) -> usize {
        if self.strings.contains(&string) {
            self.strings.iter().position(|x| x == &string).unwrap()
        } else {
            self.strings.push(string);
            self.strings.len() - 1
        }
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

    pub fn generate_statement(&mut self, ast: &'a Statement) -> Result<()> {
        match ast {
            Statement::Declaration(name, value_type, range) => {
                self.scope
                    .insert(name, (self.scope.size()?, value_type.clone()), range)?;
                self.instr(Sub(RSP, Constant(STACK_OFFSET as u64)));
            }
            Statement::Assignment(name, value, range) => {
                let (offset, variable_type) = self.scope.find(name, range)?;
                let (value_reg, value_type) = self.generate_expression(value).unwrap();

                assert_eq!(variable_type, value_type);
                self.instr(Mov(
                    RegIndirect(Rbp, offset * STACK_OFFSET),
                    Reg(value_reg, value_type.size()),
                ));

                self.free_register(value_reg);
            }
            Statement::While(cond, stmt, _range) => {
                let start_label = self.get_new_label();
                let end_label = self.get_new_label();

                self.instr(Label(start_label));

                let (register, value_type) = self.generate_expression(cond)?;

                self.instr(Cmp(Reg(register, value_type.size()), Constant(0)));
                self.instr(Jz(JmpLabel(end_label)));

                self.generate_statement(stmt)?;

                self.instr(Jmp(JmpLabel(start_label)));
                self.instr(Label(end_label));

                self.free_register(register);
            }
            Statement::If(cond, stmt, _range) => {
                let end_label = self.get_new_label();
                let (register, value_type) = self.generate_expression(cond)?;

                self.instr(Cmp(Reg(register, value_type.size()), Constant(0)));
                self.free_register(register);

                self.instr(Jz(JmpLabel(end_label)));

                self.generate_statement(stmt)?;

                self.instr(Label(end_label));
            }
            Statement::Return(expr, _range) => {
                let (value_reg, value_type) = self.generate_expression(expr).unwrap();

                self.instr(Mov(
                    Reg(Rax, value_type.size()),
                    Reg(value_reg, value_type.size()),
                ));

                self.free_register(value_reg);
            }
            Statement::Function(name, args, _ret_type, body, range) => {
                self.scope.push();
                assert!(args.len() <= 6);
                assert_eq!(self.allocated_registers.iter().filter(|x| **x).count(), 0);

                self.instr(Function(
                    if name == "main" { "_start" } else { name }.to_string(),
                ));

                self.write_prologue();

                if !args.is_empty() {
                    self.instr(Sub(RSP, Constant((STACK_OFFSET * args.len()) as u64)));
                }

                for ((arg_name, arg_type), arg_reg) in args.iter().zip(ARGUMENT_REGISTERS) {
                    let offset = self.scope.size()?;
                    self.scope
                        .insert(arg_name, (offset, arg_type.clone()), range)?;
                    self.instr(Mov(
                        RegIndirect(Rbp, offset * STACK_OFFSET),
                        Reg(arg_reg, arg_type.size()),
                    ));
                }

                self.generate_statement(body)?;

                self.write_epilogue();

                self.instr(Ret());
                self.scope.pop(range)?;

                assert_eq!(self.allocated_registers.iter().filter(|x| **x).count(), 0);
            }
            Statement::Block(stmts, scoped, range) => {
                if *scoped {
                    self.scope.push();
                }

                for stmt in stmts {
                    self.generate_statement(stmt)?;
                }

                if *scoped {
                    self.scope.pop(range)?;
                }
            }
            Statement::Expression(expr, _range) => {
                let (register, _type) = self.generate_expression(expr)?;
                self.free_register(register);
            }
        }
        Ok(())
    }

    fn generate_expression(&mut self, ast: &'a Expression) -> Result<(X86Register, Type)> {
        match ast {
            Expression::Literal(value, value_type, _range) => {
                let register = self.get_next_register();

                self.instr(Mov(Reg(register, value_type.size()), Constant(*value)));

                Ok((register, value_type.clone()))
            }
            Expression::UnaryOperator(UnaryOperatorType::Ref, expr, _range) => {
                let result_reg = self.get_next_register();

                match &**expr {
                    Expression::VariableRef(name, range) => {
                        let (offset, value_type) = self.scope.find(name, range)?;
                        let result_type = value_type.get_ref();
                        self.instr(Lea(
                            Reg(result_reg, result_type.size()),
                            RegIndirect(Rbp, offset * 16),
                        ));
                        Ok((result_reg, result_type))
                    }
                    _ => unreachable!(),
                }
            }
            Expression::UnaryOperator(op, expr, _range) => {
                let (reg, value_type) = self.generate_expression(expr)?;
                let result_reg = self.get_next_register();

                match op {
                    &UnaryOperatorType::Deref => {
                        self.instr(Mov(Reg(result_reg, value_type.size()), RegIndirect(reg, 0)));
                    }
                    _ => unreachable!(),
                }

                self.free_register(reg);
                Ok((result_reg, value_type.get_deref()))
            }
            Expression::BinaryOperator(op, left, right, _range) => {
                let (left_reg, left_type) = self.generate_expression(left)?;
                let (right_reg, right_type) = self.generate_expression(right)?;

                assert_eq!(left_type.size(), right_type.size());

                let left_op = Reg(left_reg, left_type.size());
                let right_op = Reg(right_reg, right_type.size());

                match op {
                    BinaryOperatorType::Add => {
                        self.instr(Add(left_op, right_op));
                    }
                    BinaryOperatorType::Subtract => {
                        self.instr(Sub(left_op, right_op));
                    }
                    BinaryOperatorType::Multiply => {
                        assert!(
                            left_type.size() > 8 && right_type.size() > 8,
                            "We don't support multiplication of 8 bit integers"
                        );
                        self.instr(Mul(left_op, right_op));
                    }
                    BinaryOperatorType::Divide => {
                        self.instr(Mov(Reg(Rax, left_type.size()), left_op.clone()));
                        self.instr(Div(right_op));
                        self.instr(Mov(left_op, Reg(Rax, left_type.size())));
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
                Ok((left_reg, left_type))
            }
            Expression::VariableRef(name, range) => {
                let (offset, value_type) = self.scope.find(name, range)?;
                let register = self.get_next_register();

                self.instr(Mov(
                    Reg(register, value_type.size()),
                    RegIndirect(Rbp, offset * 16),
                ));

                Ok((register, value_type))
            }
            Expression::FunctionCall(name, args, return_type, _range) => {
                assert!(args.len() <= 6);

                if name.starts_with("syscall") {
                    let syscall_num = args.get(0).unwrap();
                    let (syscall_num_reg, _syscall_num_type) =
                        self.generate_expression(syscall_num)?;

                    for arg in args.iter().skip(1).zip(&ARGUMENT_REGISTERS) {
                        let (arg_register, arg_type) = self.generate_expression(arg.0)?;
                        if arg.1.is_caller_saved() {
                            self.instr(Push(Reg(*arg.1, 64)));
                        }
                        self.instr(Mov(
                            Reg(*arg.1, arg_type.size()),
                            Reg(arg_register, arg_type.size()),
                        ));
                        self.free_register(arg_register);
                    }

                    self.instr(Mov(RAX, Reg(syscall_num_reg, 64)));
                    self.instr(Syscall());

                    self.free_register(syscall_num_reg);

                    for i in (0..args.len()).rev() {
                        if ARGUMENT_REGISTERS[i].is_caller_saved() {
                            self.instr(Pop(Reg(ARGUMENT_REGISTERS[i], 64)));
                        }
                    }

                    let result_reg = self.get_next_register();
                    self.instr(Mov(Reg(result_reg, return_type.size()), RAX));

                    Ok((result_reg, Type::UInt64()))
                } else {
                    for (expr, dest_reg) in args.iter().zip(&ARGUMENT_REGISTERS) {
                        let (arg_register, arg_type) = self.generate_expression(expr)?;

                        if dest_reg.is_caller_saved() {
                            self.instr(Push(Reg(*dest_reg, 64)));
                        }
                        self.instr(Mov(
                            Reg(*dest_reg, arg_type.size()),
                            Reg(arg_register, arg_type.size()),
                        ));
                        self.free_register(arg_register);
                    }

                    // TODO: get rid of this disgusting hack
                    if name == "printf" {
                        self.instr(Mov(RAX, Constant(0)));
                    }

                    self.instr(Call(Reference(name.to_string())));
                    let result_register = self.get_next_register();

                    if return_type != &Type::Void() {
                        self.instr(Mov(
                            Reg(result_register, return_type.size()),
                            Reg(Rax, return_type.size()),
                        ));
                    }

                    for i in (0..args.len()).rev() {
                        if ARGUMENT_REGISTERS[i].is_caller_saved() {
                            self.instr(Pop(Reg(ARGUMENT_REGISTERS[i], 64)));
                        }
                    }

                    Ok((result_register, return_type.clone()))
                }
            }
            Expression::StringLiteral(value, _range) => {
                let label = self.store_new_string(value);
                let result_reg = self.get_next_register();
                self.instr(Mov(Reg(result_reg, 64), StringLabel(label)));
                Ok((result_reg, Type::UInt8().get_ref()))
            }
            Expression::Widen(expr, widen_type, _range) => {
                let (expr_reg, expr_type) = self.generate_expression(expr)?;
                assert!(expr_type.size() < widen_type.size());

                let result_reg = self.get_next_register();
                self.instr(MovZX(
                    Reg(result_reg, widen_type.size()),
                    Reg(expr_reg, expr_type.size()),
                ));

                self.free_register(expr_reg);

                Ok((result_reg, widen_type.clone()))
            }
        }
    }

    pub fn write<T: Write>(&'a self, writer: &'a mut T) {
        writeln!(
            writer,
            r#"extern printf
extern exit
section .data
{}"#,
            self.strings
                .iter()
                .enumerate()
                .map(|(i, x)| format!("\tS{} db `{}`, 0", i, x.replace('\n', "\\n")))
                .collect::<Vec<_>>()
                .join("\n")
        )
        .unwrap();

        writeln!(
            writer,
            r#"section .text
    global _start
"#
        )
        .unwrap();
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
}
