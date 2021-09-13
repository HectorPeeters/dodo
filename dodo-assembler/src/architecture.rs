use crate::instruction::Instruction;

pub trait Architecture {
    type OutputRegister;
    type Constant;

    fn emit_instruction(instr: Instruction<Self::OutputRegister, Self::Constant>) -> Vec<u8>;
}
