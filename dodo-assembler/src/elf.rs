pub type Elf64Addr = u64;
pub type Elf64Off = u64;

pub type Elf64Half = u16;
pub type Elf64Word = u32;
pub type Elf64SWord = i32;
pub type Elf64XWord = u64;
pub type Elf64SXWord = i64;

pub struct ElfFile {
    strings: Vec<u8>,

    sections: Vec<ElfSectionHeaderEntry>,
    section_data: Vec<Vec<u8>>,

    symbols: Vec<ElfSymbol>,

    string_table_index: u16,
    text_string: u32,
    shrtrtab_string: u32,
    symtab_string: u32,
}

impl ElfFile {
    pub fn new() -> Self {
        let mut file = Self {
            strings: vec![0x00],
            sections: vec![],
            section_data: vec![],
            symbols: vec![],
            string_table_index: 0,
            text_string: 0,
            shrtrtab_string: 0,
            symtab_string: 0,
        };

        file.text_string = file.add_string(".text");
        file.shrtrtab_string = file.add_string(".shrtrtab");
        file.symtab_string = file.add_string(".symtab");

        let null_section = ElfSectionHeaderEntry {
            name: 0,
            kind: ElfSectionType::Null,
            flags: 0,
            addr: 0,
            off: 0,
            size: 0,
            link: 0,
            info: 0,
            address_align: 0,
            entry_size: 0,
        };

        file.sections.push(null_section);
        file.section_data.push(vec![]);

        let null_symbol = ElfSymbol {
            name: 0,
            info_binding: ElfSymbolBindings::Local,
            info_type: ElfSymbolType::NoType,
            other: 0,
            shndx: 0,
            value: 0,
            size: 0,
        };

        file.symbols.push(null_symbol);

        file
    }

    pub fn add_string(&mut self, string: &str) -> u32 {
        let index = self.strings.len();
        self.strings.extend_from_slice(string.as_bytes());
        self.strings.push(0);
        index as u32
    }

    pub fn add_code_section(&mut self, code: &[u8]) -> u16 {
        let header = ElfSectionHeaderEntry {
            name: self.text_string,
            kind: ElfSectionType::ProgBits,
            flags: ElfSectionFlags::ExecInstr as u64 | ElfSectionFlags::Alloc as u64,
            addr: 0,
            off: 0,
            size: code.len() as Elf64XWord,
            link: 0,
            info: 0,
            address_align: 16,
            entry_size: 0,
        };

        self.sections.push(header);
        self.section_data.push(code.to_vec());

        self.sections.len() as u16 - 1
    }

    pub fn add_shrtrtab_section(&mut self) {
        let header = ElfSectionHeaderEntry {
            name: self.shrtrtab_string,
            kind: ElfSectionType::StrTab,
            flags: ElfSectionFlags::Alloc as u64,
            addr: 0,
            off: 0,
            size: self.strings.len() as Elf64XWord,
            link: 0,
            info: 0,
            address_align: 0,
            entry_size: 0,
        };

        self.sections.push(header);
        // TODO: lots of cloning which should get removed
        self.section_data.push(self.strings.clone());

        self.string_table_index = self.sections.len() as u16 - 1;
    }

    pub fn add_symtab_section(&mut self) {
        let header = ElfSectionHeaderEntry {
            name: self.symtab_string,
            kind: ElfSectionType::SymTab,
            flags: 0,
            addr: 0,
            off: 0,
            size: self.symbols.len() as Elf64XWord * 24,
            link: 2,
            info: 1,
            address_align: 0,
            entry_size: 24,
        };
        self.sections.push(header);

        let mut data = vec![];
        for symbol in &self.symbols {
            data.extend_from_slice(&write_symbol(symbol));
        }
        self.section_data.push(data);
    }

    pub fn add_symbol(&mut self, symbol: ElfSymbol) {
        self.symbols.push(symbol);
    }

    pub fn write_elf_file(&mut self) -> Vec<u8> {
        // header
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
            shnum: self.sections.len() as u16,
            shstrndx: self.string_table_index,
        };

        let mut result = vec![];
        result.extend_from_slice(&write_elf_header(&header));

        let mut current_offset: u64 = 64 + self.sections.len() as u64 * 64;

        // section headers
        for section in &mut self.sections {
            section.off = current_offset;
            current_offset += section.size;
            result.extend_from_slice(&write_section_header(section));
        }

        for data in &self.section_data {
            result.extend_from_slice(data);
        }

        result
    }
}

impl Default for ElfFile {
    fn default() -> Self {
        Self::new()
    }
}

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

    buff.extend_from_slice(&(header.kind as u16).to_le_bytes());
    buff.extend_from_slice(&header.machine.to_le_bytes());
    buff.extend_from_slice(&header.version.to_le_bytes());
    buff.extend_from_slice(&header.entry.to_le_bytes());
    buff.extend_from_slice(&header.phoff.to_le_bytes());
    buff.extend_from_slice(&header.shoff.to_le_bytes());
    buff.extend_from_slice(&header.flags.to_le_bytes());
    buff.extend_from_slice(&header.ehsize.to_le_bytes());
    buff.extend_from_slice(&header.phentsize.to_le_bytes());
    buff.extend_from_slice(&header.phnum.to_le_bytes());
    buff.extend_from_slice(&header.shentsize.to_le_bytes());
    buff.extend_from_slice(&header.shnum.to_le_bytes());
    buff.extend_from_slice(&header.shstrndx.to_le_bytes());

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
#[repr(u64)]
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

#[derive(Copy, Clone)]
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

#[derive(Copy, Clone)]
#[repr(u32)]
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

        assert_eq!(result.len(), 64);
        assert_eq!(
            result,
            vec![
                127, 69, 76, 70, // Magic
                2,  // Class
                1,  // Data
                1,  // Version
                0,  // OsAbi
                0,  // Abi Version
                0, 0, 0, 0, 0, 0, 0, // Padding
                1, 0, // Type
                62, 0, // Machine
                1, 0, 0, 0, // Version
                0, 0, 0, 0, 0, 0, 0, 0, // Entry
                0, 0, 0, 0, 0, 0, 0, 0, // Ph Offset
                64, 0, 0, 0, 0, 0, 0, 0, // Sh Offset
                0, 0, 0, 0, // Flags
                64, 0, // Elf Header Size
                0, 0, // Program Header Entry Size
                0, 0, // Program Header Num
                64, 0, // Section Header Entry Size
                5, 0, // Section Header Num
                2, 0 // String Table Index
            ]
        );
    }
}
