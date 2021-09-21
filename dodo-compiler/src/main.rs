use dodo_assembler::elf::{ElfFile, ElfSymbol, ElfSymbolBindings, ElfSymbolType};

pub mod ast;
pub mod code_generator;
pub mod parser;
pub mod visitor;

fn main() {
    let mut elf_file = ElfFile::new();

    let code = vec![
        0xb8, 0x3c, 0x0, 0x0, 0x0, 0xbf, 0x09, 0x0, 0x0, 0x0, 0x0f, 0x05,
    ];

    let start_string = elf_file.add_string("_start");

    let code_section = elf_file.add_code_section(&code);
    elf_file.add_symbol(ElfSymbol {
        name: start_string,
        info_binding: ElfSymbolBindings::Global,
        info_type: ElfSymbolType::NoType,
        other: 0,
        shndx: code_section,
        value: 0,
        size: 0,
    });

    elf_file.add_shrtrtab_section();
    elf_file.add_symtab_section();

    use std::io::Write;
    std::fs::File::create("a.o")
        .unwrap()
        .write_all(&elf_file.write_elf_file())
        .unwrap();
}
