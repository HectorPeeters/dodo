use crate::ast::*;
use crate::visitor::{ExpressionVisitor, StatementVisitor};
use dodo_assembler::architecture::Architecture;
use dodo_assembler::{instruction::Instruction, instructionstream::InstructionStream};

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
