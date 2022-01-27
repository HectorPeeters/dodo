use std::fs::File;

use error::Result;
use parser::Parser;
use std::env;
use tokenizer::tokenize;
use x86_nasm::X86NasmGenerator;

mod ast;
mod error;
mod parser;
mod scope;
mod tokenizer;
mod types;
mod x86_instruction;
mod x86_nasm;

fn main() -> Result<()> {
    let args: Vec<_> = env::args().collect();
    let file = args.get(1).unwrap();

    let code = std::fs::read_to_string(file).unwrap();

    let tokens = tokenize(&code)?;
    let mut parser = Parser::<u64>::new(&tokens);

    let mut output = File::create("output.asm").unwrap();
    let mut generator = X86NasmGenerator::new(&mut output);

    let mut statements = vec![];

    while !parser.eof() {
        statements.push(parser.parse_function()?);
    }

    for statement in &statements {
        generator.generate_statement(statement)?;
    }

    generator.finish();

    Ok(())
}
