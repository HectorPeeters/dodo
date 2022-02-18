#![feature(int_log)]

use std::fs::File;

use error::Result;
use parser::Parser;
use std::env;
use tokenizer::tokenize;
use type_checker::TypeChecker;
use x86_nasm::X86NasmGenerator;

mod ast;
mod error;
mod parser;
mod scope;
mod tokenizer;
mod type_checker;
mod types;
mod x86_instruction;
mod x86_nasm;

fn main() -> Result<()> {
    let args: Vec<_> = env::args().collect();
    let file = args.get(1).unwrap();

    let code = std::fs::read_to_string(file).unwrap();

    let tokens = tokenize(&code, file);
    if let Err(error) = tokens {
        error.print().unwrap();
        std::process::exit(1);
    }
    let tokens = tokens.unwrap();

    let mut parser = Parser::new(&tokens, file);

    let mut output = File::create("output.asm").unwrap();
    let mut generator = X86NasmGenerator::new(file);

    let mut statements = vec![];

    while !parser.eof() {
        match parser.parse_statement() {
            Ok(statement) => statements.push(statement),
            Err(error) => {
                error.print().unwrap();
                std::process::exit(1);
            }
        }
    }

    let mut type_checker = TypeChecker::new(file);

    for statement in &mut statements {
        let type_check_result = type_checker.check(statement);
        if let Err(error) = type_check_result {
            error.print().unwrap();
            std::process::exit(1);
        }

        let generated = generator.generate_statement(statement);
        if let Err(error) = generated {
            error.print().unwrap();
            std::process::exit(1);
        }
    }

    generator.write(&mut output);

    Ok(())
}
