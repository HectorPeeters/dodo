use crate::instruction::Instruction;

pub trait Architecture {
    type OutputRegister;
    type Constant: Copy;

    fn emit_instruction(instr: Instruction<Self::OutputRegister, Self::Constant>) -> Vec<u8>;
}
