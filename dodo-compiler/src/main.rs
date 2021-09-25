use crate::{parser::Parser, visitor::*};
use dodo_assembler::{
    architecture::Architecture,
    assembler::Assembler,
    elf::{ElfFile, ElfSymbol, ElfSymbolBindings, ElfSymbolType},
    x86::X86,
};
use dodo_core::Result;

pub mod ast;
pub mod code_generator;
pub mod parser;
pub mod tokenizer;
pub mod visitor;

fn main() -> Result<()> {
    let input = "return 9 + 2;";
    let tokens = tokenizer::tokenize(input)?;
    let mut parser = Parser::<<X86 as Architecture>::Constant>::new(&tokens);

    let ast = parser.parse_statement()?;

    let mut codegen = code_generator::CodeGenerator::<X86>::new();
    codegen.visit_statement(&ast);

    let instr_stream = Assembler::allocate_registers(codegen.instruction_stream);
    let code = Assembler::assemble::<X86>(instr_stream);

    println!("{:02x?}", code);

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

    Ok(())
}
