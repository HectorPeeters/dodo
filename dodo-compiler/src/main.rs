use crate::{code_generator::CodeGenerator, visitor::StatementVisitor};
use dodo_assembler::assembler::Assembler;
use dodo_assembler::x86::X86;

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

    let new_instructionstream = Assembler::allocate_registers::<X86>(&generator.instruction_stream);

    let code = Assembler::assemble::<X86>(new_instructionstream);

    println!("{:?}", code);
}
