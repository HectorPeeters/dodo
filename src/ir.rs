use std::fmt::Display;

pub type IrString = usize;
pub type IrBlockIndex = usize;

#[derive(Debug, Copy, Clone)]
pub struct IrRegister {
    pub index: usize,
    pub size: IrRegisterSize,
}

impl Display for IrRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.index, self.size)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum IrRegisterSize {
    Byte,
    Word,
    Double,
    Quad,
}

impl From<usize> for IrRegisterSize {
    fn from(s: usize) -> Self {
        match s {
            8 => IrRegisterSize::Byte,
            16 => IrRegisterSize::Word,
            32 => IrRegisterSize::Double,
            64 => IrRegisterSize::Quad,
            _ => unreachable!("Register size {}", s),
        }
    }
}

impl Display for IrRegisterSize {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IrRegisterSize::Byte => write!(f, "b"),
            IrRegisterSize::Word => write!(f, "w"),
            IrRegisterSize::Double => write!(f, "d"),
            IrRegisterSize::Quad => write!(f, "q"),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum IrValue {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    Bool(bool),
    String(usize),
    Uninitialized(),
}

impl IrValue {
    pub fn to_string(&self, builder: &IrBuilder) -> String {
        use IrValue::*;

        match self {
            U8(x) => format!("{}", *x),
            U16(x) => format!("{}", *x),
            U32(x) => format!("{}", *x),
            U64(x) => format!("{}", *x),
            // TODO: when we have something better than printf we can switch to 'true' and 'false'
            Bool(x) => u32::from(*x).to_string(),
            String(index) => builder.strings[*index].to_string(),
            Uninitialized() => "uninitalized".to_string(),
        }
    }

    pub fn get_reg_size(&self) -> IrRegisterSize {
        use IrRegisterSize::*;

        match self {
            IrValue::U8(_) => Byte,
            IrValue::U16(_) => Word,
            IrValue::U32(_) => Double,
            IrValue::U64(_) => Quad,
            IrValue::Bool(_) => Byte,
            IrValue::String(_) => Quad,
            IrValue::Uninitialized() => unreachable!(),
        }
    }
}

impl Display for IrValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use IrValue::*;

        match self {
            U8(x) => write!(f, "{}u8", *x),
            U16(x) => write!(f, "{}u16", *x),
            U32(x) => write!(f, "{}u32", *x),
            U64(x) => write!(f, "{}u64", *x),
            Bool(x) => write!(f, "{}", if *x { "true" } else { "false" }),
            String(index) => write!(f, "{}s", *index),
            Uninitialized() => write!(f, "uninitalized"),
        }
    }
}

pub enum IrInstruction {
    Mov(IrRegister, IrRegister),
    MovImm(IrRegister, IrValue),
    MovZx(IrRegister, IrRegister),
    Add(IrRegister, IrRegister, IrRegister),
    Sub(IrRegister, IrRegister, IrRegister),
    Mul(IrRegister, IrRegister, IrRegister),
    Div(IrRegister, IrRegister, IrRegister),
    Mod(IrRegister, IrRegister, IrRegister),
    Eq(IrRegister, IrRegister, IrRegister),
    Ne(IrRegister, IrRegister, IrRegister),
    Gt(IrRegister, IrRegister, IrRegister),
    GtE(IrRegister, IrRegister, IrRegister),
    Lt(IrRegister, IrRegister, IrRegister),
    LtE(IrRegister, IrRegister, IrRegister),
    Shl(IrRegister, IrRegister, IrRegister),
    Shr(IrRegister, IrRegister, IrRegister),
    Jmp(IrBlockIndex),
    CondJmp(IrBlockIndex, IrBlockIndex, IrRegister),
    Push(IrRegister),
    Pop(IrRegister),
    Call(IrBlockIndex),
    CallExtern(String, usize),
    Ret(),
}

impl Display for IrInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use IrInstruction::*;

        match self {
            Mov(dest_reg, source_reg) => write!(f, "mov\t{dest_reg} {source_reg}"),
            MovImm(reg, value) => write!(f, "movimm\t{reg} {value}"),
            MovZx(dest_reg, src_reg) => write!(f, "movzx\t{dest_reg} {src_reg}"),
            Add(dest, a, b) => write!(f, "add\t{dest} {a} {b}"),
            Sub(dest, a, b) => write!(f, "sub\t{dest} {a} {b}"),
            Mul(dest, a, b) => write!(f, "mul\t{dest} {a} {b}"),
            Div(dest, a, b) => write!(f, "div\t{dest} {a} {b}"),
            Mod(dest, a, b) => write!(f, "mod\t{dest} {a} {b}"),
            Eq(dest, a, b) => write!(f, "eq\t{dest} {a} {b}"),
            Ne(dest, a, b) => write!(f, "ne\t{dest} {a} {b}"),
            Gt(dest, a, b) => write!(f, "gt\t{dest} {a} {b}"),
            GtE(dest, a, b) => write!(f, "gte\t{dest} {a} {b}"),
            Lt(dest, a, b) => write!(f, "lt\t{dest} {a} {b}"),
            LtE(dest, a, b) => write!(f, "lte\t{dest} {a} {b}"),
            Shl(dest, a, b) => write!(f, "shl\t{dest} {a} {b}"),
            Shr(dest, a, b) => write!(f, "shr\t{dest} {a} {b}"),
            Jmp(index) => write!(f, "jmp\t{index}"),
            CondJmp(true_index, false_index, reg) => {
                write!(f, "condjmp\t{true_index} {false_index} {reg}")
            }
            Push(value) => write!(f, "push\t{value}"),
            Pop(value) => write!(f, "pop\t{value}"),
            Call(index) => write!(f, "call\t{index}"),
            CallExtern(name, arg_count) => write!(f, "ecall\t{name}[{arg_count}]"),
            Ret() => write!(f, "ret"),
        }
    }
}

pub struct IrBlock {
    pub name: String,
    pub instructions: Vec<IrInstruction>,
}

impl IrBlock {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            instructions: vec![],
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn add_instruction(&mut self, instr: IrInstruction) {
        self.instructions.push(instr);
    }
}

impl Display for IrBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for instr in &self.instructions {
            writeln!(f, "{instr}")?;
        }

        Ok(())
    }
}

#[derive(Default)]
pub struct IrBuilder {
    pub blocks: Vec<IrBlock>,
    block_stack: Vec<IrBlockIndex>,
    pub strings: Vec<String>,
    pub ir_registers: Vec<IrRegister>,
}

impl Display for IrBuilder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "STRINGS")?;
        for (id, value) in self.strings.iter().enumerate() {
            writeln!(f, "{}: '{}", id, value.replace('\n', "\\n"))?;
        }
        writeln!(f, "\n")?;

        for (index, block) in self.blocks.iter().enumerate() {
            writeln!(f, "==== {}: {} ====", index, block.name)?;
            writeln!(f, "{block}\n")?;
        }
        Ok(())
    }
}

impl IrBuilder {
    pub fn new() -> Self {
        Self {
            blocks: vec![],
            block_stack: vec![],
            strings: vec![],
            ir_registers: vec![],
        }
    }

    pub fn new_register(&mut self, size: IrRegisterSize) -> IrRegister {
        let index = self.ir_registers.len();
        let register = IrRegister { index, size };
        self.ir_registers.push(register);
        register
    }

    pub fn add_block(&mut self, name: &str) -> IrBlockIndex {
        let block = IrBlock::new(name);
        self.blocks.push(block);
        self.blocks.len() - 1
    }

    pub fn add_instruction(&mut self, instr: IrInstruction) {
        let block_index = self.current_block();
        self.blocks[block_index].add_instruction(instr);
    }

    pub fn push_block(&mut self, block: IrBlockIndex) {
        assert!(block < self.blocks.len());
        self.block_stack.push(block);
    }

    pub fn pop_block(&mut self) {
        assert!(!self.block_stack.is_empty());
        self.block_stack.remove(self.block_stack.len() - 1);
    }

    pub fn add_string(&mut self, string: String) -> usize {
        self.strings.push(string);
        self.strings.len() - 1
    }

    fn current_block(&self) -> IrBlockIndex {
        assert!(!self.block_stack.is_empty());
        *self.block_stack.last().unwrap()
    }

    pub fn find_block_index(&mut self, name: &str) -> Option<IrBlockIndex> {
        self.blocks.iter().position(|b| b.name == name)
    }

    pub fn can_jump_blocks(&self, block_index: IrBlockIndex) -> Vec<IrBlockIndex> {
        let mut result = vec![];

        let mut add = |value| {
            if !result.contains(value) {
                result.push(*value);
            }
        };

        for instr in &self.blocks[block_index].instructions {
            match instr {
                IrInstruction::Jmp(target) => add(target),
                IrInstruction::CondJmp(true_target, false_target, _) => {
                    add(true_target);
                    add(false_target);
                }
                _ => {}
            }
        }

        result
    }

    pub fn get_dot_graph(&self) -> String {
        let mut buffer = String::new();
        buffer.push_str("digraph G {\n");

        for (index, block) in self.blocks.iter().enumerate() {
            let instruction_text = block
                .instructions
                .iter()
                .map(|x| format!("{x}\\l"))
                .collect::<Vec<_>>()
                .join("");

            buffer.push_str(&format!(
                "\t{} [label=\"{}: {}\\n{}\", shape=box];\n",
                index,
                index,
                block.name(),
                instruction_text
            ));

            for reachable_block in self.can_jump_blocks(index) {
                buffer.push_str(&format!("\t{index} -> {reachable_block}\n"));
            }
        }

        buffer.push('}');
        buffer
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_block() {
        let mut builder = IrBuilder::new();
        let main_block = builder.add_block("main");
        assert_eq!(main_block, 0);
    }

    #[test]
    fn jump_block() {
        let mut builder = IrBuilder::new();
        let main_block = builder.add_block("main");

        let target_block = builder.add_block("target");
        builder.push_block(main_block);
        builder.add_instruction(IrInstruction::Jmp(target_block));
        builder.pop_block();

        assert_eq!(builder.can_jump_blocks(main_block), vec![1]);
        assert_eq!(builder.can_jump_blocks(target_block), vec![]);
    }

    #[test]
    fn cyclic_jump_block() {
        let mut builder = IrBuilder::new();
        let main_block = builder.add_block("main");

        let target_block = builder.add_block("target");
        builder.push_block(main_block);
        builder.add_instruction(IrInstruction::Jmp(target_block));
        builder.pop_block();
        builder.push_block(target_block);
        builder.add_instruction(IrInstruction::Jmp(main_block));
        builder.pop_block();

        assert_eq!(builder.can_jump_blocks(main_block), vec![target_block]);
        assert_eq!(builder.can_jump_blocks(target_block), vec![main_block]);
    }

    #[test]
    fn add_two_numbers() {
        use IrInstruction::*;

        let mut builder = IrBuilder::new();

        let main_block = builder.add_block("main");

        builder.push_block(main_block);
        let a_register = builder.new_register(IrRegisterSize::Double);
        builder.add_instruction(MovImm(a_register, IrValue::U32(12)));
        let b_register = builder.new_register(IrRegisterSize::Double);
        builder.add_instruction(MovImm(b_register, IrValue::U32(13)));

        let result_register = builder.new_register(IrRegisterSize::Double);
        builder.add_instruction(Add(result_register, a_register, b_register));
        builder.pop_block();
    }

    #[test]
    fn add_number_function() {
        use IrInstruction::*;

        let mut builder = IrBuilder::new();

        let add_block = builder.add_block("add");
        builder.push_block(add_block);

        let a_reg = builder.new_register(IrRegisterSize::Double);
        builder.add_instruction(Pop(a_reg));

        let b_reg = builder.new_register(IrRegisterSize::Double);
        builder.add_instruction(Pop(b_reg));

        let result_reg = builder.new_register(IrRegisterSize::Double);
        builder.add_instruction(Add(result_reg, a_reg, b_reg));

        builder.add_instruction(Push(result_reg));
        builder.add_instruction(Ret());

        builder.pop_block();

        let main_block = builder.add_block("main");
        builder.push_block(main_block);

        let arg1_reg = builder.new_register(IrRegisterSize::Double);
        builder.add_instruction(MovImm(arg1_reg, IrValue::U32(12)));

        let arg2_reg = builder.new_register(IrRegisterSize::Double);
        builder.add_instruction(MovImm(arg2_reg, IrValue::U32(13)));

        builder.add_instruction(Push(arg1_reg));
        builder.add_instruction(Push(arg2_reg));
        builder.add_instruction(Call(add_block));

        let final_result_reg = builder.new_register(IrRegisterSize::Double);
        builder.add_instruction(Pop(final_result_reg));

        builder.pop_block();
    }
}
