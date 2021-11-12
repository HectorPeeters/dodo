use crate::visitor::{ExpressionVisitor, StatementVisitor};
use dodo_assembler::architecture::Architecture;
use dodo_assembler::{instruction::Instruction, instructionstream::InstructionStream};
use dodo_parser::ast::{BinaryOperatorType, Expression, Statement};

type Register = u32;

#[derive(Debug)]
pub struct CodeGenerator<A: Architecture> {
    pub instruction_stream: InstructionStream<Register, A::Constant>,
    next_register: Register,
}

impl<A: Architecture> CodeGenerator<A> {
    pub fn new() -> Self {
        Self {
            instruction_stream: InstructionStream::new(),
            next_register: 0,
        }
    }

    fn new_reg(&mut self) -> Register {
        let reg = self.next_register;
        self.next_register += 1;
        reg
    }
}

impl<A: Architecture> Default for CodeGenerator<A> {
    fn default() -> Self {
        Self::new()
    }
}

impl<A: Architecture> ExpressionVisitor<Register, A::Constant> for CodeGenerator<A> {
    fn visit_expression(&mut self, expr: &Expression<A::Constant>) -> Register {
        match expr {
            Expression::Constant(value, _value_type) => {
                let reg = self.new_reg();
                self.instruction_stream
                    .instr(Instruction::MovImm(reg, *value));
                reg
            }
            Expression::BinaryOperator(op, left, right) => {
                let left_reg = self.visit_expression(left);
                let right_reg = self.visit_expression(right);
                let output_reg = self.new_reg();
                let instruction = match op {
                    BinaryOperatorType::Add => Instruction::Add(left_reg, right_reg, output_reg),
                    _ => unreachable!(),
                };
                self.instruction_stream.instr(instruction);
                output_reg
            }
            _ => unreachable!(),
        }
    }
}

impl<A: Architecture> StatementVisitor<(), A::Constant> for CodeGenerator<A> {
    fn visit_statement(&mut self, stmt: &Statement<A::Constant>) {
        match stmt {
            Statement::Return(expr) => {
                let reg = self.visit_expression(expr);
                self.instruction_stream.instr(Instruction::Ret(reg));
            }
            _ => unreachable!(),
        }
    }
}
