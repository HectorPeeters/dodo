use crate::architecture::Architecture;
use crate::instruction::Instruction;

pub struct X86 {}

#[derive(Clone, Copy)]
pub enum X86Register {
    Eax,
    Ecx,
    Edx,
    Ebx,
    Esp,
    Ebp,
    Esi,
    Edi,
}

impl X86Register {
    pub fn as_byte(&self) -> u8 {
        *self as u8
    }

    pub fn from_index(i: u32) -> Self {
        use X86Register::*;

        match i {
            0 => Eax,
            1 => Ecx,
            2 => Edx,
            3 => Ebx,
            _ => todo!(),
        }
    }
}

impl Architecture for X86 {
    type OutputRegister = X86Register;
    type Constant = u32;

    fn emit_instruction(instr: Instruction<Self::OutputRegister, Self::Constant>) -> Vec<u8> {
        use Instruction::*;
        match instr {
            MovImm(r, c) => {
                let mut result = vec![0xb8 + r.as_byte()];
                result.extend(c.to_le_bytes().to_vec());
                result
            }
            Ret(r) => {
                vec![
                    0x89,
                    0xc7 + r.as_byte(), // mov %reg, %edi
                    0xb8,
                    0x3c,
                    0x00,
                    0x00,
                    0x00, // mov $60, %eax
                    0x0f,
                    0x05, // syscall
                ]
            }
            _ => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{assembler::Assembler, instructionstream::InstructionStream};
    use Instruction::*;
    use X86Register::*;

    #[test]
    fn x86_mov() {
        let mut stream = InstructionStream::new();
        for reg in [Eax, Ecx, Edx, Ebx, Esi, Edi, Esp, Ebp] {
            stream.instr(MovImm(reg, 0x12345678));
        }
        let assembled = Assembler::assemble::<X86>(stream);
        assert_eq!(
            assembled,
            vec![
                0xb8, 0x78, 0x56, 0x34, 0x12, // eax
                0xb9, 0x78, 0x56, 0x34, 0x12, // ecx
                0xba, 0x78, 0x56, 0x34, 0x12, // edx
                0xbb, 0x78, 0x56, 0x34, 0x12, // ebx
                0xbe, 0x78, 0x56, 0x34, 0x12, // esi
                0xbf, 0x78, 0x56, 0x34, 0x12, // edi
                0xbc, 0x78, 0x56, 0x34, 0x12, // esp
                0xbd, 0x78, 0x56, 0x34, 0x12, // ebp
            ]
        );
    }
}
