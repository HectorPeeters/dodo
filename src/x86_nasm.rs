use std::collections::HashMap;
use std::io::Write;

use crate::{
    ast::{BinaryOperatorType, Expression, Statement},
    error::Result,
};

pub struct X86NasmGenerator<'a, T: Write> {
    writer: &'a mut T,
    label_index: usize,
    current_variables: HashMap<String, usize>,
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
            current_variables: HashMap::new(),
            allocated_registers: [false; 8],
        }
    }

    fn get_next_register(&mut self) -> usize {
        match self.allocated_registers.iter().position(|x| !x) {
            Some(reg) => {
                self.allocated_registers[reg] = true;
                reg
            }
            None => unreachable!(),
        }
    }

    fn free_register(&mut self, reg: usize) {
        assert!(self.allocated_registers[reg]);
        self.allocated_registers[reg] = false;
    }

    fn get_register_name(index: usize) -> &'static str {
        match index {
            0 => "r8",
            1 => "r9",
            2 => "r10",
            3 => "r11",
            4 => "r12",
            5 => "r13",
            6 => "r14",
            7 => "r15",
            _ => unreachable!(),
        }
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
                self.current_variables
                    .insert(name.clone(), self.current_variables.len());
                writeln!(self.writer, "\tsub rsp, 16").unwrap();
            }
            Statement::Assignment(name, value) => {
                let offset = self.current_variables[name];
                let value_reg = self.generate_expression(value).unwrap();

                if offset == 0 {
                    writeln!(
                        self.writer,
                        "\tmov [rbp], {}",
                        Self::get_register_name(value_reg)
                    )
                    .unwrap();
                } else {
                    writeln!(
                        self.writer,
                        "\tmov [rbp - {}], {}",
                        offset * 16,
                        Self::get_register_name(value_reg)
                    )
                    .unwrap();
                }

                self.free_register(value_reg);
            }
            Statement::While(cond, stmt) => {
                let start_label = self.get_new_label();
                let end_label = self.get_new_label();
                writeln!(self.writer, "L{}:", start_label).unwrap();
                let register = Self::get_register_name(self.generate_expression(cond)?);
                writeln!(self.writer, "\ttest {}, {}", register, register).unwrap();
                writeln!(self.writer, "\tjz L{}", end_label).unwrap();
                self.generate_statement(stmt)?;
                writeln!(self.writer, "\tjmp L{}", start_label).unwrap();
                writeln!(self.writer, "L{}:", end_label).unwrap();
            }
            Statement::Return(expr) => {
                let value_reg = self.generate_expression(expr).unwrap();

                writeln!(
                    self.writer,
                    "\tmov rax, {}",
                    Self::get_register_name(value_reg)
                )
                .unwrap();

                self.free_register(value_reg);
            }
            Statement::Function(name, _arg_types, _ret_type, body) => {
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

    pub fn generate_expression(&mut self, ast: &Expression<u64>) -> Result<usize> {
        match ast {
            Expression::Constant(value, _type) => {
                let register = self.get_next_register();
                writeln!(
                    self.writer,
                    "\tmov {}, {}",
                    Self::get_register_name(register),
                    value
                )
                .unwrap();
                Ok(register)
            }
            Expression::BinaryOperator(op, left, right) => {
                let left_reg = self.generate_expression(left)?;
                let right_reg = self.generate_expression(right)?;
                match op {
                    BinaryOperatorType::Add => {
                        writeln!(
                            self.writer,
                            "\tadd {}, {}",
                            Self::get_register_name(left_reg),
                            Self::get_register_name(right_reg)
                        )
                        .unwrap();
                        self.free_register(right_reg);
                        Ok(left_reg)
                    }
                    BinaryOperatorType::Subtract => {
                        writeln!(
                            self.writer,
                            "\tsub {}, {}",
                            Self::get_register_name(left_reg),
                            Self::get_register_name(right_reg)
                        )
                        .unwrap();
                        self.free_register(right_reg);
                        Ok(left_reg)
                    }
                    _ => unreachable!(),
                }
            }
            Expression::VariableRef(name) => {
                let offset = self.current_variables[name];
                let register = self.get_next_register();
                if offset == 0 {
                    writeln!(
                        self.writer,
                        "\tmov {}, [rbp]",
                        Self::get_register_name(register),
                    )
                    .unwrap();
                } else {
                    writeln!(
                        self.writer,
                        "\tmov {}, [rbp - {}]",
                        Self::get_register_name(register),
                        offset * 16,
                    )
                    .unwrap();
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
                        "\tmov rax, 0\n\tmov rdi, fmt\n\tmov rsi, {}\n\tcall printf",
                        Self::get_register_name(arg_register)
                    )
                    .unwrap();
                    self.free_register(arg_register);
                } else {
                    if args.len() == 1 {
                        let arg_register = self.generate_expression(&args[0])?;
                        writeln!(
                            self.writer,
                            "\tmov rdi, {}",
                            Self::get_register_name(arg_register)
                        )
                        .unwrap();
                    }

                    writeln!(self.writer, "\tcall {}", name).unwrap();
                    writeln!(
                        self.writer,
                        "\tmov {}, rax",
                        Self::get_register_name(result_register)
                    )
                    .unwrap();
                }
                Ok(result_register)
            }
            _ => unreachable!(),
        }
    }
}
