use crate::visitor::*;
use dodo_assembler::{
    assembler::Assembler,
    elf::{ElfFile, ElfSymbol, ElfSymbolBindings, ElfSymbolType},
    x86::X86,
};

pub mod ast;
pub mod code_generator;
pub mod parser;
pub mod visitor;

fn main() {
    let ast = parser::parse_statement::<()>("return 9;").unwrap().1;

    let mut codegen = code_generator::CodeGenerator::new();
    codegen.visit_statement(&ast);

    let instr_stream = Assembler::allocate_registers(codegen.instruction_stream);
    let code = Assembler::assemble::<X86>(instr_stream);

    let mut elf_file = ElfFile::new();

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
