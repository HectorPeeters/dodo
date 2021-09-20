use crate::ast::*;
use crate::visitor::{ExpressionVisitor, StatementVisitor};
use dodo_assembler::{instruction::Instruction, instructionstream::InstructionStream};

type Register = u32;
type Constant = u32;

#[derive(Debug)]
pub struct CodeGenerator {
    pub instruction_stream: InstructionStream<Register, Constant>,
    next_register: Register,
}

impl CodeGenerator {
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

impl Default for CodeGenerator {
    fn default() -> Self {
        Self::new()
    }
}

impl ExpressionVisitor<Register> for CodeGenerator {
    fn visit_binary_operator(&mut self, _binary: &BinaryOperatorExpression) -> Register {
        unreachable!()
    }

    fn visit_unary_operator(&mut self, _unary: &UnaryOperatorExpression) -> Register {
        unreachable!()
    }

    fn visit_function_call(&mut self, _function: &FunctionCallExpression) -> Register {
        unreachable!()
    }

    fn visit_cast(&mut self, _cast: &CastExpression) -> Register {
        unreachable!()
    }

    fn visit_constant(&mut self, constant: &ConstantExpression) -> Register {
        let reg = self.new_reg();
        self.instruction_stream
            .instr(Instruction::MovImm(reg, constant.value));
        reg
    }
}

impl StatementVisitor<()> for CodeGenerator {
    fn visit_block(&mut self, _block: &BlockStatement) {
        todo!()
    }

    fn visit_declaration(&mut self, _declaration: &DeclarationStatement) {
        todo!()
    }

    fn visit_assignment(&mut self, _assignment: &AssignmentStatement) {
        todo!()
    }

    fn visit_return(&mut self, ret: &ReturnStatement) {
        let reg = self.visit_expression(&ret.value);
        self.instruction_stream.instr(Instruction::Ret(reg));
    }

    fn visit_function(&mut self, _function: &FunctionStatement) {
        todo!()
    }
}
