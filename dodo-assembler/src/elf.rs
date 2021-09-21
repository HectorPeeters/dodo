pub type Elf64Addr = u64;
pub type Elf64Off = u64;

pub type Elf64Half = u16;
pub type Elf64Word = u32;
pub type Elf64SWord = i32;
pub type Elf64XWord = u64;
pub type Elf64SXWord = i64;

pub struct ElfFile {}

#[derive(Clone, Copy)]
pub enum ElfClass {
    ElfClass32 = 1,
    ElfClass64 = 2,
}

#[derive(Clone, Copy)]
pub enum ElfData {
    ElfData2Lsb = 1,
    ElfData2Msb = 2,
}

#[derive(Clone, Copy)]
pub enum ElfOsAbi {
    ElfOsAbiSysv = 0,
    ElfOsAbiHpUx = 1,
    ElfOsAbiStandalone = 255,
}

#[derive(Clone, Copy)]
pub enum ElfType {
    None = 0,
    Rel = 1,
    Exec = 2,
    Dyn = 3,
    Core = 4,
    LoOs = 0xFE00,
    HiOs = 0xFEFF,
    LoProc = 0xFF00,
    HiProc = 0xFFFF,
}

pub struct ElfHeader {
    pub ident_class: ElfClass,
    pub ident_data: ElfData,
    pub ident_version: u8,
    pub ident_osabi: ElfOsAbi,
    pub ident_abi_version: u8,
    pub kind: ElfType,
    pub machine: Elf64Half,
    pub version: Elf64Word,
    pub entry: Elf64Addr,
    pub phoff: Elf64Off,
    pub shoff: Elf64Off,
    pub flags: Elf64Word,
    pub ehsize: Elf64Half,
    pub phentsize: Elf64Half,
    pub phnum: Elf64Half,
    pub shentsize: Elf64Half,
    pub shnum: Elf64Half,
    pub shstrndx: Elf64Half,
}

pub fn write_elf_header(header: &ElfHeader) -> Vec<u8> {
    let mut buff = vec![
        0x7F,
        0x45,
        0x4C,
        0x46,
        header.ident_class as u8,
        header.ident_data as u8,
        header.ident_version as u8,
        header.ident_osabi as u8,
        header.ident_abi_version as u8,
    ];
    buff.append(&mut vec![0; 7]);

    buff.extend_from_slice(&mut (header.kind as u16).to_le_bytes());
    buff.extend_from_slice(&mut header.machine.to_le_bytes());
    buff.extend_from_slice(&mut header.version.to_le_bytes());
    buff.extend_from_slice(&mut header.entry.to_le_bytes());
    buff.extend_from_slice(&mut header.phoff.to_le_bytes());
    buff.extend_from_slice(&mut header.shoff.to_le_bytes());
    buff.extend_from_slice(&mut header.flags.to_le_bytes());
    buff.extend_from_slice(&mut header.ehsize.to_le_bytes());
    buff.extend_from_slice(&mut header.phentsize.to_le_bytes());
    buff.extend_from_slice(&mut header.phnum.to_le_bytes());
    buff.extend_from_slice(&mut header.shentsize.to_le_bytes());
    buff.extend_from_slice(&mut header.shnum.to_le_bytes());
    buff.extend_from_slice(&mut header.shstrndx.to_le_bytes());

    assert_eq!(buff.len(), 64);

    buff
}

#[derive(Clone, Copy)]
pub enum ElfSectionType {
    Null = 0,
    ProgBits = 1,
    SymTab = 2,
    StrTab = 3,
    Rela = 4,
    Hash = 5,
    Dynamic = 6,
    Note = 7,
    NoBits = 8,
    Rel = 9,
    ShLib = 10,
    DynSym = 11,
    LoOs = 0x60000000,
    HiOs = 0x6FFFFFFF,
    LoProc = 0x70000000,
    HiProc = 0x7FFFFFFF,
}

#[derive(Clone, Copy)]
pub enum ElfSectionFlags {
    Write = 0x1,
    Alloc = 0x2,
    ExecInstr = 0x4,
    MaskOs = 0x0F000000,
    MaskProc = 0xF0000000,
}

pub struct ElfSectionHeaderEntry {
    pub name: Elf64Word,
    pub kind: ElfSectionType,
    pub flags: Elf64XWord,
    pub addr: Elf64Addr,
    pub off: Elf64Off,
    pub size: Elf64XWord,
    pub link: Elf64Word,
    pub info: Elf64Word,
    pub address_align: Elf64XWord,
    pub entry_size: Elf64XWord,
}

pub fn write_section_header(header: &ElfSectionHeaderEntry) -> Vec<u8> {
    let mut res = vec![];

    res.extend_from_slice(&header.name.to_le_bytes());
    res.extend_from_slice(&(header.kind as u32).to_le_bytes());
    res.extend_from_slice(&(header.flags as u64).to_le_bytes());
    res.extend_from_slice(&header.addr.to_le_bytes());
    res.extend_from_slice(&header.off.to_le_bytes());
    res.extend_from_slice(&header.size.to_le_bytes());
    res.extend_from_slice(&(header.link as u32).to_le_bytes());
    res.extend_from_slice(&(header.info as u32).to_le_bytes());
    res.extend_from_slice(&header.address_align.to_le_bytes());
    res.extend_from_slice(&header.entry_size.to_le_bytes());

    assert_eq!(res.len(), 64);

    res
}

#[derive(Copy, Clone)]
pub enum ElfSymbolBindings {
    Local = 0,
    Global = 1,
    Weak = 2,
    LoOs = 10,
    HiOs = 12,
    LoProc = 13,
    HiProc = 15,
}

#[derive(Copy, Clone)]
pub enum ElfSymbolType {
    NoType = 0,
    Object = 1,
    Func = 2,
    Section = 3,
    File = 4,
    LoOs = 10,
    HiOs = 12,
    LoProc = 13,
    HiProc = 15,
}

pub struct ElfSymbol {
    pub name: Elf64Word,
    pub info_binding: ElfSymbolBindings,
    pub info_type: ElfSymbolType,
    pub other: u8,
    pub shndx: Elf64Half,
    pub value: Elf64Addr,
    pub size: Elf64XWord,
}

pub fn write_symbol(symbol: &ElfSymbol) -> Vec<u8> {
    let mut res = vec![];

    res.extend_from_slice(&symbol.name.to_le_bytes());
    let info: u8 =
        (((symbol.info_binding as u8) & 0b1111) << 4) | ((symbol.info_type as u8) & 0b1111);
    res.push(info);
    res.push(symbol.other);
    res.extend_from_slice(&symbol.shndx.to_le_bytes());
    res.extend_from_slice(&symbol.value.to_le_bytes());
    res.extend_from_slice(&symbol.size.to_le_bytes());

    assert_eq!(res.len(), 24);

    res
}

pub enum ElfProgramHeaderType {
    Null = 0,
    Load = 1,
    Dynamic = 2,
    Interp = 3,
    Note = 4,
    ShLib = 5,
    Phdr = 6,
    LoOs = 0x60000000,
    HiOs = 0x6FFFFFFF,
    LoProc = 0x70000000,
    HiProc = 0x7FFFFFFF,
}

pub enum ElfProgramHeaderFlags {
    X = 0x1,
    W = 0x2,
    R = 0x4,
    MaskOs = 0x00FF0000,
    MaskProc = 0xFF000000,
}

pub struct ElfProgramHeader {
    pub kind: ElfProgramHeaderType,
    pub flags: Elf64Word,
    pub offset: Elf64Off,
    pub vaddr: Elf64Addr,
    pub paddr: Elf64Addr,
    pub filesz: Elf64XWord,
    pub memsz: Elf64XWord,
    pub align: Elf64XWord,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn elf_write_header() {
        let header = ElfHeader {
            ident_class: ElfClass::ElfClass64,
            ident_data: ElfData::ElfData2Lsb,
            ident_version: 1,
            ident_osabi: ElfOsAbi::ElfOsAbiSysv,
            ident_abi_version: 0,
            kind: ElfType::Rel,
            machine: 0x3E,
            version: 0x1,
            entry: 0,
            phoff: 0,
            shoff: 64,
            flags: 0,
            ehsize: 64,
            phentsize: 0,
            phnum: 0,
            shentsize: 64,
            shnum: 5,
            shstrndx: 2,
        };
        let result = write_elf_header(&header);
        use std::io::Write;
        std::fs::File::create("a2.o")
            .unwrap()
            .write(&result)
            .unwrap();
    }
}
