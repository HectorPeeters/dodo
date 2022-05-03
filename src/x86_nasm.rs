use std::io::Write;

use crate::{
    ast::{
        BinaryOperatorType, ConsumingAstVisitor, Expression, Statement, TypedExpression,
        TypedStatement, UnaryOperatorType,
    },
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

pub type ScopeLocation = usize;

pub struct X86NasmGenerator<'a> {
    instructions: Vec<X86Instruction>,
    label_index: usize,
    scope: Scope<'a, ScopeLocation>,
    allocated_registers: [bool; GENERAL_PURPOSE_REGISTER_OFFSET],
    strings: Vec<String>,
    current_function_end_label: usize,
}

impl<'a> X86NasmGenerator<'a> {
    pub fn new(source_file: &'a str) -> Self {
        Self {
            instructions: vec![],
            label_index: 0,
            scope: Scope::new(source_file),
            allocated_registers: [false; GENERAL_PURPOSE_REGISTER_OFFSET],
            strings: vec![],
            current_function_end_label: 0,
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

    fn store_new_string(&mut self, string: &str) -> usize {
        let position = self.strings.iter().position(|x| x == string);
        match position {
            Some(x) => x,
            None => {
                self.strings.push(string.to_string());
                self.strings.len() - 1
            }
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

impl<'a> ConsumingAstVisitor<Type, (), X86Register> for X86NasmGenerator<'a> {
    fn visit_statement(&mut self, statement: TypedStatement) -> Result<()> {
        match statement {
            Statement::Declaration(name, _value_type, _, range) => {
                self.scope.insert(&name, self.scope.size()?, &range)?;
                self.instr(Sub(RSP, Constant(STACK_OFFSET as u64)));
            }
            Statement::Assignment(name, value, value_type, range) => {
                let offset = self.scope.find(&name, &range)?;
                let value_reg = self.visit_expression(value)?;

                self.instr(Mov(
                    RegIndirect(Rbp, offset * STACK_OFFSET),
                    Reg(value_reg, value_type.size()),
                ));

                self.free_register(value_reg);
            }
            Statement::While(cond, stmt, _, _range) => {
                let start_label = self.get_new_label();
                let end_label = self.get_new_label();

                let cond_size = cond.data().size();

                self.instr(Label(start_label));

                let register = self.visit_expression(cond)?;

                self.instr(Cmp(Reg(register, cond_size), Constant(0)));
                self.instr(Jz(JmpLabel(end_label)));

                self.visit_statement(*stmt)?;

                self.instr(Jmp(JmpLabel(start_label)));
                self.instr(Label(end_label));

                self.free_register(register);
            }
            Statement::If(cond, stmt, _, _range) => {
                let end_label = self.get_new_label();
                let cond_size = cond.data().size();
                let cond_register = self.visit_expression(cond)?;

                self.instr(Cmp(Reg(cond_register, cond_size), Constant(0)));
                self.free_register(cond_register);

                self.instr(Jz(JmpLabel(end_label)));

                self.visit_statement(*stmt)?;

                self.instr(Label(end_label));
            }
            Statement::Return(expr, _, _range) => {
                let expr_size = expr.data().size();
                let value_reg = self.visit_expression(expr).unwrap();

                self.instr(Mov(Reg(Rax, expr_size), Reg(value_reg, expr_size)));
                self.instr(Jmp(JmpLabel(self.current_function_end_label)));

                self.free_register(value_reg);
            }
            Statement::Function(name, args, _ret_type, body, _value_type, range) => {
                self.scope.push();
                assert!(args.len() <= 6);
                assert_eq!(self.allocated_registers.iter().filter(|x| **x).count(), 0);

                assert_eq!(self.current_function_end_label, 0);
                self.current_function_end_label = self.get_new_label();

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
                    self.scope.insert(arg_name, offset, &range)?;
                    self.instr(Mov(
                        RegIndirect(Rbp, offset * STACK_OFFSET),
                        Reg(arg_reg, arg_type.size()),
                    ));
                }

                self.visit_statement(*body)?;

                self.instr(Label(self.current_function_end_label));
                self.current_function_end_label = 0;

                self.write_epilogue();

                self.instr(Ret());
                self.scope.pop(&range)?;

                assert_eq!(self.allocated_registers.iter().filter(|x| **x).count(), 0);
            }
            Statement::Block(stmts, scoped, _, range) => {
                if scoped {
                    self.scope.push();
                }

                for stmt in stmts {
                    self.visit_statement(stmt)?;
                }

                if scoped {
                    self.scope.pop(&range)?;
                }
            }
            Statement::Expression(expr, _expr_type, _range) => {
                let register = self.visit_expression(expr)?;
                self.free_register(register);
            }
        }
        Ok(())
    }

    fn visit_expression(&mut self, expression: TypedExpression) -> Result<X86Register> {
        match expression {
            Expression::Literal(value, value_type, _range) => {
                let register = self.get_next_register();

                self.instr(Mov(Reg(register, value_type.size()), Constant(value)));

                Ok(register)
            }
            Expression::UnaryOperator(UnaryOperatorType::Ref, expr, _, _range) => {
                let result_reg = self.get_next_register();

                match &*expr {
                    Expression::VariableRef(name, value_type, range) => {
                        let offset = self.scope.find(name, range)?;
                        let result_type = value_type.clone().get_ref();
                        self.instr(Lea(
                            Reg(result_reg, result_type.size()),
                            RegIndirect(Rbp, offset * 16),
                        ));
                        Ok(result_reg)
                    }
                    _ => unreachable!(),
                }
            }
            Expression::UnaryOperator(op, expr, _, _range) => {
                let expr_size = expr.data().size();
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
                let left_size = left.data().size();
                let right_size = right.data().size();
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
                        self.instr(Div(right_op));
                        self.instr(Mov(left_op, Reg(Rax, left_size)));
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
                let offset = self.scope.find(&name, &range)?;
                let register = self.get_next_register();

                self.instr(Mov(
                    Reg(register, value_type.size()),
                    RegIndirect(Rbp, offset * 16),
                ));

                Ok(register)
            }
            Expression::FunctionCall(name, mut args, return_type, _range) => {
                assert!(args.len() <= 6);

                if name.starts_with("syscall") {
                    let syscall_num = args.remove(0);
                    let arg_count = args.len();
                    let syscall_num_reg = self.visit_expression(syscall_num)?;

                    for arg in args.into_iter().zip(&ARGUMENT_REGISTERS) {
                        let arg_size = arg.0.data().size();
                        let arg_register = self.visit_expression(arg.0)?;
                        if arg.1.is_caller_saved() {
                            self.instr(Push(Reg(*arg.1, 64)));
                        }
                        self.instr(Mov(Reg(*arg.1, arg_size), Reg(arg_register, arg_size)));
                        self.free_register(arg_register);
                    }

                    self.instr(Mov(RAX, Reg(syscall_num_reg, 64)));
                    self.instr(Syscall());

                    self.free_register(syscall_num_reg);

                    for i in (0..arg_count).rev() {
                        if ARGUMENT_REGISTERS[i].is_caller_saved() {
                            self.instr(Pop(Reg(ARGUMENT_REGISTERS[i], 64)));
                        }
                    }

                    let result_reg = self.get_next_register();
                    self.instr(Mov(
                        Reg(result_reg, return_type.size()),
                        Reg(Rax, return_type.size()),
                    ));

                    Ok(result_reg)
                } else {
                    let arg_count = args.len();

                    for (expr, dest_reg) in args.into_iter().zip(&ARGUMENT_REGISTERS) {
                        let arg_size = expr.data().size();
                        let arg_register = self.visit_expression(expr)?;

                        if dest_reg.is_caller_saved() {
                            self.instr(Push(Reg(*dest_reg, 64)));
                        }
                        self.instr(Mov(Reg(*dest_reg, arg_size), Reg(arg_register, arg_size)));
                        self.free_register(arg_register);
                    }

                    // TODO: get rid of this disgusting hack
                    if name == "printf" {
                        self.instr(Mov(RAX, Constant(0)));
                    }

                    self.instr(Call(Reference(name)));
                    let result_register = self.get_next_register();

                    if return_type != Type::Void() {
                        self.instr(Mov(
                            Reg(result_register, return_type.size()),
                            Reg(Rax, return_type.size()),
                        ));
                    }

                    for i in (0..arg_count).rev() {
                        if ARGUMENT_REGISTERS[i].is_caller_saved() {
                            self.instr(Pop(Reg(ARGUMENT_REGISTERS[i], 64)));
                        }
                    }

                    Ok(result_register)
                }
            }
            Expression::StringLiteral(value, _, _range) => {
                let label = self.store_new_string(&value);
                let result_reg = self.get_next_register();
                self.instr(Mov(Reg(result_reg, 64), StringLabel(label)));
                Ok(result_reg)
            }
            Expression::Widen(expr, widen_type, _range) => {
                let expr_size = expr.data().size();
                assert!(expr_size < widen_type.size());
                let expr_reg = self.visit_expression(*expr)?;

                let result_reg = self.get_next_register();
                self.instr(MovZX(
                    Reg(result_reg, widen_type.size()),
                    Reg(expr_reg, expr_size),
                ));

                self.free_register(expr_reg);

                Ok(result_reg)
            }
        }
    }
}
