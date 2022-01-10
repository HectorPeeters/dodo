use std::{fmt, io::Write};

use crate::{
    ast::{BinaryOperatorType, Expression, Statement},
    error::Result,
    scope::Scope,
};

#[derive(Debug, Clone, Copy)]
pub enum Register {
    Rax = 0,
    Rcx,
    Rbx,
    Rsi,
    Rdi,
    Rsp,
    Rbp,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

impl From<usize> for Register {
    fn from(x: usize) -> Self {
        use Register::*;
        match x {
            0 => Rax,
            1 => Rcx,
            2 => Rbx,
            3 => Rsi,
            4 => Rdi,
            5 => Rsp,
            6 => Rbp,
            7 => R8,
            8 => R9,
            9 => R10,
            10 => R11,
            11 => R12,
            12 => R13,
            13 => R14,
            14 => R15,
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Register::*;
        write!(
            f,
            "{}",
            match self {
                Rax => "rax",
                Rcx => "rcx",
                Rbx => "rbx",
                Rsi => "rsi",
                Rdi => "rdi",
                Rsp => "rsp",
                Rbp => "rbp",
                R8 => "r8",
                R9 => "r9",
                R10 => "r10",
                R11 => "r11",
                R12 => "r12",
                R13 => "r13",
                R14 => "r14",
                R15 => "r15",
            }
        )
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ScopeLocation {
    Reg(Register),
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

    fn get_next_register(&mut self) -> Register {
        match self.allocated_registers.iter().position(|x| !x) {
            Some(reg) => {
                self.allocated_registers[reg] = true;
                Register::from(reg + 8)
            }
            None => unreachable!(),
        }
    }

    fn free_register(&mut self, reg: Register) {
        assert!(self.allocated_registers[reg as usize - 8]);
        self.allocated_registers[reg as usize - 8] = false;
    }

    fn get_new_label(&mut self) -> usize {
        let result = self.label_index;
        self.label_index += 1;
        result
    }

    fn write_prologue(&mut self) {
        writeln!(self.writer, "\tpush rbp").unwrap();
        writeln!(self.writer, "\tpush rbp").unwrap();
        writeln!(self.writer, "\tmov rbp, rsp").unwrap();
    }

    fn write_epilogue(&mut self) {
        writeln!(self.writer, "\tmov rsp, rbp").unwrap();
        writeln!(self.writer, "\tpop rbp").unwrap();
        writeln!(self.writer, "\tpop rbp").unwrap();
    }

    pub fn generate_statement(&mut self, ast: &Statement<u64>) -> Result<()> {
        match ast {
            Statement::Declaration(name, _type) => {
                self.scope
                    .insert(name, ScopeLocation::Stack(self.scope.len()?))?;
                writeln!(self.writer, "\tsub rsp, 16").unwrap();
            }
            Statement::Assignment(name, value) => {
                let scope_entry = self.scope.find(name)?;
                let value_reg = self.generate_expression(value).unwrap();

                match scope_entry {
                    ScopeLocation::Stack(offset) => {
                        if offset == 0 {
                            writeln!(self.writer, "\tmov [rbp], {}", value_reg).unwrap();
                        } else {
                            writeln!(self.writer, "\tmov [rbp - {}], {}", offset * 16, value_reg)
                                .unwrap();
                        }
                    }
                    ScopeLocation::Reg(reg) => {
                        writeln!(self.writer, "\tmov {}, {}", reg, value_reg).unwrap();
                    }
                }

                self.free_register(value_reg);
            }
            Statement::While(cond, stmt) => {
                let start_label = self.get_new_label();
                let end_label = self.get_new_label();
                writeln!(self.writer, "L{}:", start_label).unwrap();
                let register = self.generate_expression(cond)?;
                writeln!(self.writer, "\ttest {}, {}", register, register).unwrap();
                writeln!(self.writer, "\tjz L{}", end_label).unwrap();
                self.generate_statement(stmt)?;
                writeln!(self.writer, "\tjmp L{}", start_label).unwrap();
                writeln!(self.writer, "L{}:", end_label).unwrap();
            }
            Statement::If(cond, stmt) => {
                let end_label = self.get_new_label();
                let register = self.generate_expression(cond)?;
                writeln!(self.writer, "\ttest {}, {}", register, register).unwrap();
                writeln!(self.writer, "\tjz L{}", end_label).unwrap();
                self.generate_statement(stmt)?;
                writeln!(self.writer, "L{}:", end_label).unwrap();
            }
            Statement::Return(expr) => {
                let value_reg = self.generate_expression(expr).unwrap();

                writeln!(self.writer, "\tmov rax, {}", value_reg).unwrap();
                self.free_register(value_reg);
            }
            Statement::Function(name, args, _ret_type, body) => {
                self.scope.push();
                assert!(args.len() <= 1);

                if args.len() == 1 {
                    self.scope
                        .insert(&args[0].0, ScopeLocation::Reg(Register::Rdi))?;
                }

                writeln!(
                    self.writer,
                    "{}:",
                    if name == "main" { "_start" } else { name }
                )
                .unwrap();
                self.write_prologue();
                self.generate_statement(body)?;
                self.write_epilogue();

                if name == "main" {
                    writeln!(self.writer, "\tmov rdi, 0\n\tcall exit",).unwrap();
                } else {
                    writeln!(self.writer, "\tret").unwrap();
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

    pub fn generate_expression(&mut self, ast: &Expression<u64>) -> Result<Register> {
        match ast {
            Expression::Constant(value, _type) => {
                let register = self.get_next_register();
                writeln!(self.writer, "\tmov {}, {}", register, value).unwrap();
                Ok(register)
            }
            Expression::BinaryOperator(op, left, right) => {
                let left_reg = self.generate_expression(left)?;
                let right_reg = self.generate_expression(right)?;
                match op {
                    BinaryOperatorType::Add => {
                        writeln!(self.writer, "\tadd {}, {}", left_reg, right_reg).unwrap();
                        self.free_register(right_reg);
                        Ok(left_reg)
                    }
                    BinaryOperatorType::Subtract => {
                        writeln!(self.writer, "\tsub {}, {}", left_reg, right_reg).unwrap();
                        self.free_register(right_reg);
                        Ok(left_reg)
                    }
                    _ => unreachable!(),
                }
            }
            Expression::VariableRef(name) => {
                let scope_location = self.scope.find(name)?;
                let register = self.get_next_register();

                match scope_location {
                    ScopeLocation::Stack(offset) => {
                        if offset == 0 {
                            writeln!(self.writer, "\tmov {}, [rbp]", register,).unwrap();
                        } else {
                            writeln!(self.writer, "\tmov {}, [rbp - {}]", register, offset * 16,)
                                .unwrap();
                        }
                    }
                    ScopeLocation::Reg(reg) => {
                        writeln!(self.writer, "\tmov {}, {}", register, reg).unwrap();
                    }
                }
                Ok(register)
            }
            Expression::FunctionCall(name, args) => {
                assert!(args.len() <= 1);

                let result_register = self.get_next_register();

                if name == "print" {
                    let arg_register = self.generate_expression(&args[0])?;
                    writeln!(
                        self.writer,
                        "\tmov rax, 0\n\tpush rdi\n\tmov rdi, fmt\n\tmov rsi, {}\n\tcall printf\n\tpop rdi", 
                        arg_register
                    )
                    .unwrap();
                    self.free_register(arg_register);
                } else {
                    if args.len() == 1 {
                        let arg_register = self.generate_expression(&args[0])?;
                        writeln!(self.writer, "\tpush rdi").unwrap();
                        writeln!(self.writer, "\tmov rdi, {}", arg_register).unwrap();
                    }

                    writeln!(self.writer, "\tcall {}", name).unwrap();
                    writeln!(self.writer, "\tmov {}, rax", result_register).unwrap();

                    if args.len() == 1 {
                        writeln!(self.writer, "\tpop rdi").unwrap();
                    }
                }
                Ok(result_register)
            }
            _ => unreachable!(),
        }
    }
}
