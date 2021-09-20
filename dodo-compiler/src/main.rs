use crate::{code_generator::CodeGenerator, visitor::StatementVisitor};
use dodo_assembler::assembler::Assembler;
use dodo_assembler::x86::X86;
use object::write::{Relocation, StandardSection, Symbol, SymbolSection};
use object::{SymbolFlags, SymbolKind, SymbolScope};

pub mod ast;
pub mod code_generator;
pub mod parser;
pub mod visitor;

fn main() {
    let ast = parser::parse_statement::<()>("return 12;").unwrap().1;
    println!("{:?}", ast);

    let mut generator = CodeGenerator::new();
    generator.visit_statement(&ast);

    println!("{:?}", generator.instruction_stream);

    let new_instructionstream = Assembler::allocate_registers(generator.instruction_stream);

    let code = Assembler::assemble::<X86>(new_instructionstream);

    let mut obj = object::write::Object::new(
        object::BinaryFormat::Elf,
        object::Architecture::X86_64,
        object::Endianness::Little,
    );

    let text = obj.section_id(StandardSection::Text);
    let start_offset = obj.append_section_data(text, &code, 8);
    let start_symbol = obj.add_symbol(Symbol {
        name: b"_start".to_vec(),
        value: start_offset,
        size: 128,
        kind: SymbolKind::Text,
        scope: SymbolScope::Linkage,
        weak: false,
        section: SymbolSection::Section(text),
        flags: SymbolFlags::None,
    });
    obj.add_relocation(
        text,
        Relocation {
            offset: 0,
            size: 64,
            kind: object::RelocationKind::Absolute,
            encoding: object::RelocationEncoding::Generic,
            symbol: start_symbol,
            addend: 0,
        },
    )
    .unwrap();

    let output = obj.write().unwrap();
    std::fs::write(std::path::Path::new("a.o"), output).unwrap();

    println!("{:02x?}", code);
}
