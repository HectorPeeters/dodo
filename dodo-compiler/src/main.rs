use dodo_assembler::elf::{
    write_elf_header, write_section_header, write_symbol, Elf64XWord, ElfClass, ElfData, ElfHeader,
    ElfOsAbi, ElfSectionFlags, ElfSectionHeaderEntry, ElfSectionType, ElfSymbol, ElfSymbolBindings,
    ElfSymbolType, ElfType,
};

pub mod ast;
pub mod code_generator;
pub mod parser;
pub mod visitor;

fn main() {
    let code = vec![
        0xb8, 0x3c, 0x0, 0x0, 0x0, 0xbf, 0x00, 0x0, 0x0, 0x0, 0x0f, 0x05,
    ];

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
        shnum: 4,
        shstrndx: 2,
    };

    let mut section_names = vec![0x0];
    section_names.extend_from_slice(".text".as_bytes());
    section_names.push(0);
    section_names.extend_from_slice(".shrtrtab".as_bytes());
    section_names.push(0);
    section_names.extend_from_slice(".symtab".as_bytes());
    section_names.push(0);
    section_names.extend_from_slice("_start".as_bytes());
    section_names.push(0);

    let null_symbol = ElfSymbol {
        name: 0,
        info_binding: ElfSymbolBindings::Local,
        info_type: ElfSymbolType::NoType,
        other: 0,
        shndx: 0,
        value: 0,
        size: 0,
    };

    let start_symbol = ElfSymbol {
        name: 25,
        info_binding: ElfSymbolBindings::Global,
        info_type: ElfSymbolType::NoType,
        other: 0,
        shndx: 1,
        value: 0,
        size: 0,
    };

    let mut symbol_table = vec![];
    symbol_table.extend(write_symbol(&null_symbol));
    symbol_table.extend(write_symbol(&start_symbol));

    let null_section_header = ElfSectionHeaderEntry {
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

    let code_section_header = ElfSectionHeaderEntry {
        name: 1,
        kind: ElfSectionType::ProgBits,
        flags: ElfSectionFlags::ExecInstr as u64 | ElfSectionFlags::Alloc as u64,
        addr: 0,
        off: 64 + 64 * 4,
        size: code.len() as Elf64XWord,
        link: 0,
        info: 0,
        address_align: 16,
        entry_size: 0,
    };

    let strtab_section_header = ElfSectionHeaderEntry {
        name: 7,
        kind: ElfSectionType::StrTab,
        flags: ElfSectionFlags::Alloc as u64,
        addr: 0,
        off: 64 + 64 * 4 + code.len() as u64,
        size: section_names.len() as Elf64XWord,
        link: 0,
        info: 0,
        address_align: 0,
        entry_size: 0,
    };

    let symtab_section_header = ElfSectionHeaderEntry {
        name: 17,
        kind: ElfSectionType::SymTab,
        flags: 0,
        addr: 0,
        off: 64 + 64 * 4 + code.len() as u64 + section_names.len() as u64,
        size: 24 * 2,
        link: 2,
        info: 1,
        address_align: 0,
        entry_size: 24,
    };

    let mut full_file = write_elf_header(&header);
    full_file.extend(write_section_header(&null_section_header));
    full_file.extend(write_section_header(&code_section_header));
    full_file.extend(write_section_header(&strtab_section_header));
    full_file.extend(write_section_header(&symtab_section_header));
    full_file.extend(code);
    full_file.extend(section_names);
    full_file.extend(symbol_table);

    use std::io::Write;
    std::fs::File::create("a2.o")
        .unwrap()
        .write_all(&full_file)
        .unwrap();
}
