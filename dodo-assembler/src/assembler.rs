use crate::architecture::Architecture;
use crate::instructionstream::InstructionStream;

pub struct Assembler {}

impl Assembler {
    pub fn 
    pub fn assemble<A: Architecture>(
        stream: InstructionStream<A::OutputRegister, A::Constant>,
    ) -> Vec<u8> {
        let mut result = vec![];
        for instr in stream {
            let bytes: Vec<u8> = A::emit_instruction(instr);
            result.extend(bytes);
        }
        result
    }
}
