use crate::architecture::Architecture;
use crate::instruction::Instruction;
use crate::instructionstream::InstructionStream;
use crate::x86::X86Register;

pub struct Assembler {}

impl Assembler {
    pub fn allocate_registers<C: Copy>(
        stream: InstructionStream<u32, C>,
    ) -> InstructionStream<X86Register, C> {
        assert!(stream.get_max_registers() <= 4);

        let mut new_stream = InstructionStream::new();

        for instruction in stream {
            use Instruction::*;

            match instruction {
                MovImm(r, c) => {
                    new_stream.instr(Instruction::MovImm(X86Register::from_index(r), c))
                }
                Ret(r) => new_stream.instr(Instruction::Ret(X86Register::from_index(r))),
                _ => todo!(),
            }
        }

        new_stream
    }

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
