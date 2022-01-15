use std::fmt;

#[derive(Debug, Clone, Copy)]
pub enum X86Register {
    Rax = 0,
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

impl From<usize> for X86Register {
    fn from(x: usize) -> Self {
        use X86Register::*;
        match x {
            0 => Rax,
            1 => Rcx,
            2 => Rdx,
            3 => Rsi,
            4 => Rdi,
            5 => Rsp,
            6 => Rbp,
            7 => R8,
            8 => R9,
            9 => R10,
            10 => R11,
            11 => R12,
            12 => R13,
            13 => R14,
            14 => R15,
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

pub enum X86Operand {
    Reg(X86Register),
    RegIndirect(X86Register, usize),
    Constant(u64),
    OpLabel(usize),
    StringRef(String),
}

impl fmt::Display for X86Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use X86Operand::*;

        match self {
            Reg(r) => write!(f, "{r}"),
            RegIndirect(r, offset) => write!(f, "[{r} - {offset}]"),
            Constant(x) => write!(f, "0x{x:x}"),
            OpLabel(l) => write!(f, "L{l}"),
            StringRef(x) => write!(f, "{x}"),
        }
    }
}

pub enum X86Instruction {
    Push(X86Operand),
    Pop(X86Operand),
    Mov(X86Operand, X86Operand),
    Add(X86Operand, X86Operand),
    Sub(X86Operand, X86Operand),
    Test(X86Operand, X86Operand),
    Jz(X86Operand),
    Jmp(X86Operand),
    Call(X86Operand),
    Ret(),
    Syscall(),
    Label(usize),
    Func(String),
}

impl fmt::Display for X86Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use X86Instruction::*;
        match self {
            Push(x) => write!(f, "push {x}"),
            Pop(x) => write!(f, "pop {x}"),
            Mov(d, s) => write!(f, "mov {d}, {s}"),
            Add(d, s) => write!(f, "add {d}, {s}"),
            Sub(d, s) => write!(f, "sub {d}, {s}"),
            Test(d, s) => write!(f, "test {d}, {s}"),
            Jz(x) => write!(f, "jz {x}"),
            Jmp(x) => write!(f, "jmp {x}"),
            Call(x) => write!(f, "call {x}"),
            Ret() => write!(f, "ret"),
            Syscall() => write!(f, "syscall"),
            Label(l) => write!(f, "L{l}:"),
            Func(x) => write!(f, "{x}:"),
        }
    }
}

impl X86Instruction {
    pub fn should_indent(&self) -> bool {
        !matches!(self, X86Instruction::Label(_) | X86Instruction::Func(_))
    }
}
