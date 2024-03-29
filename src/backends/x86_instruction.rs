use std::fmt;

pub const RAX: X86Operand = X86Operand::Reg(X86Register::Rax, 64);
pub const RDX: X86Operand = X86Operand::Reg(X86Register::Rdx, 64);
pub const RBP: X86Operand = X86Operand::Reg(X86Register::Rbp, 64);
pub const RSP: X86Operand = X86Operand::Reg(X86Register::Rsp, 64);

#[derive(Debug, Clone, Copy)]
pub enum X86Register {
    Rax = 0,
    Rbx,
    Rcx,
    Rdx,
    Rsi,
    Rdi,
    Rsp,
    Rbp,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

impl X86Register {
    pub fn is_caller_saved(&self) -> bool {
        use X86Register::*;
        !matches!(self, Rbp | Rbx | R12 | R13 | R14 | R15)
    }

    pub fn get_string(&self, size: usize) -> &'static str {
        const REGISTER_NAMES: [[&str; 16]; 4] = [
            [
                "al", "bl", "cl", "dl", "sil", "dil", "spl", "bpl", "r8b", "r9b", "r10b", "r11b",
                "r12b", "r13b", "r14b", "r15b",
            ],
            [
                "ax", "bx", "cx", "dx", "si", "di", "sp", "bp", "r8w", "r9w", "r10w", "r11w",
                "r12w", "r13w", "r14w", "r15w",
            ],
            [
                "eax", "ebx", "ecx", "edx", "esi", "edi", "esp", "ebp", "r8d", "r9d", "r10d",
                "r11d", "r12d", "r13d", "r14d", "r15d",
            ],
            [
                "rax", "rbx", "rcx", "rdx", "rsi", "rdi", "rsp", "rbp", "r8", "r9", "r10", "r11",
                "r12", "r13", "r14", "r15",
            ],
        ];

        REGISTER_NAMES[(size.ilog2() - 3) as usize][*self as usize]
    }
}

impl From<usize> for X86Register {
    fn from(x: usize) -> Self {
        use X86Register::*;
        match x {
            0 => Rax,
            1 => Rbx,
            2 => Rcx,
            3 => Rdx,
            4 => Rsi,
            5 => Rdi,
            6 => Rsp,
            7 => Rbp,
            8 => R8,
            9 => R9,
            10 => R10,
            11 => R11,
            12 => R12,
            13 => R13,
            14 => R14,
            15 => R15,
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for X86Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use X86Register::*;

        write!(
            f,
            "{}",
            match self {
                Rax => "rax",
                Rbx => "rbx",
                Rcx => "rcx",
                Rdx => "rdx",
                Rsi => "rsi",
                Rdi => "rdi",
                Rsp => "rsp",
                Rbp => "rbp",
                R8 => "r8",
                R9 => "r9",
                R10 => "r10",
                R11 => "r11",
                R12 => "r12",
                R13 => "r13",
                R14 => "r14",
                R15 => "r15",
            }
        )
    }
}

#[derive(Clone)]
pub enum X86Operand {
    Reg(X86Register, usize),
    RegIndirect(X86Register, usize, bool),
    Constant(u64),
    Label(usize),
    LabelIndirect(usize),
    Reference(String),
}

impl fmt::Display for X86Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use X86Operand::*;

        match self {
            Reg(r, s) => write!(f, "{}", r.get_string(*s)),
            RegIndirect(r, offset, negative) => {
                if *negative {
                    write!(f, "[{r} - {offset}]")
                } else {
                    write!(f, "[{r} + {offset}]")
                }
            }
            Constant(x) => write!(f, "0x{x:x}"),
            Label(l) => write!(f, "L{l}"),
            LabelIndirect(l) => write!(f, "[L{l}]"),
            Reference(x) => write!(f, "{x}"),
        }
    }
}

pub enum X86Instruction {
    Push(X86Operand),
    Pop(X86Operand),
    Mov(X86Operand, X86Operand),
    MovZX(X86Operand, X86Operand),
    Lea(X86Operand, X86Operand),
    Add(X86Operand, X86Operand),
    Sub(X86Operand, X86Operand),
    Mul(X86Operand, X86Operand),
    Div(X86Operand),
    Shl(X86Operand, X86Operand),
    Shr(X86Operand, X86Operand),
    Cmp(X86Operand, X86Operand),
    SetZ(X86Operand),
    SetNZ(X86Operand),
    SetL(X86Operand),
    SetLE(X86Operand),
    SetG(X86Operand),
    SetGE(X86Operand),
    Or(X86Operand, X86Operand),
    And(X86Operand, X86Operand),
    Jz(X86Operand),
    Jmp(X86Operand),
    Call(X86Operand),
    Ret(),
    Syscall(),
    LabelDef(usize),
    Section(String),
    Global(String),
    Extern(String),
    Function(String),
    Db(Vec<u8>),
    Dw(Vec<u16>),
    Dd(Vec<u32>),
    Dq(Vec<u64>),
}
impl fmt::Display for X86Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use X86Instruction::*;
        match self {
            Push(x) => write!(f, "push {x}"),
            Pop(x) => write!(f, "pop {x}"),
            Mov(d, s) => write!(f, "mov {d}, {s}"),
            MovZX(d, s) => write!(f, "movzx {d}, {s}"),
            Lea(d, s) => write!(f, "lea {d}, {s}"),
            Add(d, s) => write!(f, "add {d}, {s}"),
            Sub(d, s) => write!(f, "sub {d}, {s}"),
            Mul(d, s) => write!(f, "imul {d}, {s}"),
            Div(x) => write!(f, "div {x}"),
            Shl(a, b) => write!(f, "shl {a}, {b}"),
            Shr(a, b) => write!(f, "shr {a}, {b}"),
            Cmp(a, b) => write!(f, "cmp {a}, {b}"),
            SetZ(x) => write!(f, "setz {x}"),
            SetNZ(x) => write!(f, "setnz {x}"),
            SetL(x) => write!(f, "setl {x}"),
            SetLE(x) => write!(f, "setle {x}"),
            SetG(x) => write!(f, "setg {x}"),
            SetGE(x) => write!(f, "setge {x}"),
            Or(d, s) => write!(f, "or {d}, {s}"),
            And(d, s) => write!(f, "and {d}, {s}"),
            Jz(x) => write!(f, "jz {x}"),
            Jmp(x) => write!(f, "jmp {x}"),
            Call(x) => write!(f, "call {x}"),
            Ret() => write!(f, "ret"),
            Syscall() => write!(f, "syscall"),
            LabelDef(l) => write!(f, "L{l}:"),
            Section(name) => write!(f, "section {name}"),
            Global(name) => write!(f, "global {name}"),
            Extern(name) => write!(f, "extern {name}"),
            Function(x) => write!(f, "{x}:"),
            Db(bytes) => write!(
                f,
                "db {}",
                bytes
                    .iter()
                    .map(|x| format!("{x:#04x}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Dw(bytes) => write!(
                f,
                "dw {}",
                bytes
                    .iter()
                    .map(|x| format!("{x:#06x}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Dd(bytes) => write!(
                f,
                "dd {}",
                bytes
                    .iter()
                    .map(|x| format!("{x:#08x}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Dq(bytes) => write!(
                f,
                "dq {}",
                bytes
                    .iter()
                    .map(|x| format!("{x:#10x}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

impl X86Instruction {
    pub fn should_indent(&self) -> bool {
        !matches!(
            self,
            X86Instruction::LabelDef(_) | X86Instruction::Function(_)
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn register_names() {
        let regs: Vec<String> = (0..=15)
            .map(|i| {
                let register: X86Register = i.into();
                format!("{register}")
            })
            .collect();

        assert_eq!(
            regs,
            vec![
                "rax".to_string(),
                "rbx".to_string(),
                "rcx".to_string(),
                "rdx".to_string(),
                "rsi".to_string(),
                "rdi".to_string(),
                "rsp".to_string(),
                "rbp".to_string(),
                "r8".to_string(),
                "r9".to_string(),
                "r10".to_string(),
                "r11".to_string(),
                "r12".to_string(),
                "r13".to_string(),
                "r14".to_string(),
                "r15".to_string()
            ]
        );
    }
}
