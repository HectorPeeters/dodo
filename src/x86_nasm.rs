use std::io::Write;

use crate::{
    ast::{BinaryOperatorType, Expression, Statement},
    error::Result,
    scope::Scope,
    x86_instruction::{X86Instruction, X86Operand, X86Register},
};

use X86Instruction::*;
use X86Operand::*;
use X86Register::*;

const ARGUMENT_REGISTERS: [X86Register; 6] = [Rdi, Rsi, Rdx, Rcx, R8, R9];
const GENERAL_PURPOSE_REGISTER_OFFSET: usize = 10;

#[derive(Debug, Clone, Copy)]
pub enum ScopeLocation {
    Reg(X86Register),
    Stack(usize),
}

pub struct X86NasmGenerator<'a, T: Write> {
    writer: &'a mut T,
    instructions: Vec<X86Instruction>,
    label_index: usize,
    scope: Scope<ScopeLocation>,
    allocated_registers: [bool; GENERAL_PURPOSE_REGISTER_OFFSET],
}

impl<'a, T: Write> X86NasmGenerator<'a, T> {
    pub fn new(writer: &'a mut T) -> Self {
        Self {
            writer,
            instructions: vec![],
            label_index: 0,
            scope: Scope::new(),
            allocated_registers: [false; GENERAL_PURPOSE_REGISTER_OFFSET],
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

    fn occupy_register(&mut self, reg: X86Register) {
        if (reg as usize) < GENERAL_PURPOSE_REGISTER_OFFSET {
            return;
        }
        assert!(!self.allocated_registers[reg as usize - GENERAL_PURPOSE_REGISTER_OFFSET]);
        self.allocated_registers[reg as usize - GENERAL_PURPOSE_REGISTER_OFFSET] = true;
    }

    fn free_register(&mut self, reg: X86Register) {
        if (reg as usize) < GENERAL_PURPOSE_REGISTER_OFFSET {
            return;
        }
        assert!(self.allocated_registers[reg as usize - GENERAL_PURPOSE_REGISTER_OFFSET]);
        self.allocated_registers[reg as usize - GENERAL_PURPOSE_REGISTER_OFFSET] = false;
    }

    fn get_new_label(&mut self) -> usize {
        let result = self.label_index;
        self.label_index += 1;
        result
    }

    fn instr(&mut self, instr: X86Instruction) {
        self.instructions.push(instr);
    }

    fn write_prologue(&mut self) {
        self.instr(Push(Reg(Rbp)));
        self.instr(Push(Reg(Rbp)));
        self.instr(Mov(Reg(Rbp), Reg(Rsp)));
    }

    fn write_epilogue(&mut self) {
        self.instr(Mov(Reg(Rsp), Reg(Rbp)));
        self.instr(Pop(Reg(Rbp)));
        self.instr(Pop(Reg(Rbp)));
    }

    pub fn generate_statement(&mut self, ast: &Statement<u64>) -> Result<()> {
        match ast {
            Statement::Declaration(name, _type) => {
                self.scope
                    .insert(name, ScopeLocation::Stack(self.scope.len()?))?;
                self.instr(Sub(Reg(Rsp), Constant(16)));
            }
            Statement::Assignment(name, value) => {
                let scope_entry = self.scope.find(name)?;
                let value_reg = self.generate_expression(value).unwrap();

                match scope_entry {
                    ScopeLocation::Stack(offset) => {
                        self.instr(Mov(RegIndirect(Rbp, offset * 16), Reg(value_reg)));
                    }
                    ScopeLocation::Reg(reg) => {
                        self.instr(Mov(Reg(reg), Reg(value_reg)));
                    }
                }

                self.free_register(value_reg);
            }
            Statement::While(cond, stmt) => {
                let start_label = self.get_new_label();
                let end_label = self.get_new_label();

                self.instr(Label(start_label));

                let register = self.generate_expression(cond)?;

                self.instr(Test(Reg(register), Reg(register)));
                self.instr(Jz(OpLabel(end_label)));

                self.generate_statement(stmt)?;

                self.instr(Jmp(OpLabel(start_label)));
                self.instr(Label(end_label));

                self.free_register(register);
            }
            Statement::If(cond, stmt) => {
                let end_label = self.get_new_label();
                let register = self.generate_expression(cond)?;

                self.instr(Test(Reg(register), Reg(register)));
                self.free_register(register);

                self.instr(Jz(OpLabel(end_label)));

                self.generate_statement(stmt)?;

                self.instr(Label(end_label));
            }
            Statement::Return(expr) => {
                let value_reg = self.generate_expression(expr).unwrap();

                self.instr(Mov(Reg(Rax), Reg(value_reg)));

                self.free_register(value_reg);
            }
            Statement::Function(name, args, _ret_type, body) => {
                self.scope.push();
                assert!(args.len() <= 6);

                for ((arg_name, _arg_type), arg_reg) in args.iter().zip(ARGUMENT_REGISTERS) {
                    self.occupy_register(arg_reg);
                    self.scope.insert(&arg_name, ScopeLocation::Reg(arg_reg))?;
                }

                self.instr(Function(
                    if name == "main" { "_start" } else { name }.to_string(),
                ));

                self.write_prologue();
                self.generate_statement(body)?;
                self.write_epilogue();

                self.instr(Ret());
                self.scope.pop()?;
            }
            Statement::Block(stmts) => {
                self.scope.push();
                for stmt in stmts {
                    self.generate_statement(stmt)?;
                }
                self.scope.pop()?;
            }
            Statement::Expression(expr) => {
                let register = self.generate_expression(expr)?;
                self.free_register(register);
            }
        }
        Ok(())
    }

    fn generate_expression(&mut self, ast: &Expression<u64>) -> Result<X86Register> {
        match ast {
            Expression::Literal(value, _type) => {
                let register = self.get_next_register();

                self.instr(Mov(Reg(register), Constant(*value)));

                Ok(register)
            }
            Expression::BinaryOperator(op, left, right) => {
                let left_reg = self.generate_expression(left)?;
                let right_reg = self.generate_expression(right)?;

                match op {
                    BinaryOperatorType::Add => {
                        self.instr(Add(Reg(left_reg), Reg(right_reg)));
                    }
                    BinaryOperatorType::Subtract => {
                        self.instr(Sub(Reg(left_reg), Reg(right_reg)));
                    }
                    _ => unreachable!(),
                }

                self.free_register(right_reg);
                Ok(left_reg)
            }
            Expression::VariableRef(name) => {
                let scope_location = self.scope.find(name)?;
                let register = self.get_next_register();

                match scope_location {
                    ScopeLocation::Stack(offset) => {
                        self.instr(Mov(Reg(register), RegIndirect(Rbp, offset * 16)));
                    }
                    ScopeLocation::Reg(reg) => {
                        self.instr(Mov(Reg(register), Reg(reg)));
                        self.free_register(reg);
                    }
                }

                Ok(register)
            }
            Expression::FunctionCall(name, args) => {
                assert!(args.len() <= 6);

                let result_register = self.get_next_register();

                if name == "print" {
                    let arg_register = self.generate_expression(&args[0])?;

                    self.instr(Mov(Reg(Rax), Constant(0)));
                    self.instr(Push(Reg(Rdi)));
                    self.instr(Mov(Reg(Rdi), StringRef("fmt".to_string())));
                    self.instr(Mov(Reg(Rsi), Reg(arg_register)));
                    self.instr(Call(StringRef("printf".to_string())));
                    self.instr(Pop(Reg(Rdi)));

                    self.free_register(arg_register);
                } else if name.starts_with("syscall") {
                    let syscall_num = args.get(0).unwrap();
                    let syscall_num_reg = self.generate_expression(syscall_num)?;

                    for arg in args.iter().skip(1).zip(&ARGUMENT_REGISTERS) {
                        let arg_register = self.generate_expression(arg.0)?;
                        self.instr(Push(Reg(*arg.1)));
                        self.instr(Mov(Reg(*arg.1), Reg(arg_register)));
                        self.free_register(arg_register);
                    }

                    self.instr(Mov(Reg(Rax), Reg(syscall_num_reg)));
                    self.instr(Syscall());

                    self.free_register(syscall_num_reg);

                    for i in (0..args.len()).rev() {
                        self.instr(Pop(Reg(ARGUMENT_REGISTERS[i])));
                    }
                } else {
                    for (expr, dest_reg) in args.iter().zip(&ARGUMENT_REGISTERS) {
                        let arg_register = self.generate_expression(expr)?;

                        self.instr(Push(Reg(*dest_reg)));
                        self.instr(Mov(Reg(*dest_reg), Reg(arg_register)));
                        self.free_register(arg_register);
                    }

                    self.instr(Call(StringRef(name.to_string())));
                    self.instr(Mov(Reg(result_register), Reg(Rax)));

                    for i in (0..args.len()).rev() {
                        self.instr(Pop(Reg(ARGUMENT_REGISTERS[i])));
                    }
                }
                Ok(result_register)
            }
            _ => unreachable!(),
        }
    }

    pub fn prepare(&mut self) {
        writeln!(
            self.writer,
            r#"extern printf
extern exit
section .data
    fmt: db "%u", 10, 0

section .text
    global _start
"#
        )
        .unwrap();
    }

    pub fn finish(&mut self) {
        writeln!(
            self.writer,
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
