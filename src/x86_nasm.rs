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

#[derive(Debug, Clone, Copy)]
pub enum ScopeLocation {
    Reg(X86Register),
    Stack(usize),
}

pub struct X86NasmGenerator<'a, T: Write> {
    writer: &'a mut T,
    label_index: usize,
    scope: Scope<ScopeLocation>,
    allocated_registers: [bool; 8],
}

impl<'a, T: Write> X86NasmGenerator<'a, T> {
    pub fn new(writer: &'a mut T) -> Self {
        writeln!(
            writer,
            "extern printf\nextern exit\nsection .data\n\tfmt: db \"%u\", 10, 0\n\nsection .text\n\tglobal _start"
        )
        .unwrap();
        Self {
            writer,
            label_index: 0,
            scope: Scope::new(),
            allocated_registers: [false; 8],
        }
    }

    fn get_next_register(&mut self) -> X86Register {
        match self.allocated_registers.iter().position(|x| !x) {
            Some(reg) => {
                self.allocated_registers[reg] = true;
                X86Register::from(reg + 8)
            }
            None => unreachable!(),
        }
    }

    fn free_register(&mut self, reg: X86Register) {
        assert!(self.allocated_registers[reg as usize - 8]);
        self.allocated_registers[reg as usize - 8] = false;
    }

    fn get_new_label(&mut self) -> usize {
        let result = self.label_index;
        self.label_index += 1;
        result
    }

    fn instr(&mut self, instr: X86Instruction) {
        writeln!(
            self.writer,
            "{}{}",
            if instr.should_indent() { "\t" } else { "" },
            instr
        )
        .unwrap();
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
            }
            Statement::If(cond, stmt) => {
                let end_label = self.get_new_label();
                let register = self.generate_expression(cond)?;

                self.instr(Test(Reg(register), Reg(register)));
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
                assert!(args.len() <= 1);

                if args.len() == 1 {
                    self.scope
                        .insert(&args[0].0, ScopeLocation::Reg(X86Register::Rdi))?;
                }

                self.instr(Func(
                    if name == "main" { "_start" } else { name }.to_string(),
                ));

                self.write_prologue();
                self.generate_statement(body)?;
                self.write_epilogue();

                if name == "main" {
                    self.instr(Mov(Reg(Rdi), Constant(0)));
                    self.instr(Call(StringRef("exit".to_string())));
                } else {
                    self.instr(Ret());
                }
                self.scope.pop()?;
            }
            Statement::Block(stmts) => {
                for stmt in stmts {
                    self.generate_statement(stmt)?;
                }
            }
            Statement::Expression(expr) => {
                self.generate_expression(expr)?;
            }
        }
        Ok(())
    }

    pub fn generate_expression(&mut self, ast: &Expression<u64>) -> Result<X86Register> {
        match ast {
            Expression::Constant(value, _type) => {
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
                    }
                }

                Ok(register)
            }
            Expression::FunctionCall(name, args) => {
                assert!(args.len() <= 1);

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
                } else {
                    if args.len() == 1 {
                        let arg_register = self.generate_expression(&args[0])?;
                        self.instr(Push(Reg(Rdi)));
                        self.instr(Mov(Reg(Rdi), Reg(arg_register)));
                    }

                    self.instr(Call(StringRef(name.to_string())));
                    self.instr(Mov(Reg(result_register), Reg(Rax)));

                    if args.len() == 1 {
                        self.instr(Pop(Reg(Rdi)));
                    }
                }
                Ok(result_register)
            }
            _ => unreachable!(),
        }
    }
}
