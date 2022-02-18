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

fn unwrap_or_error<T>(result: Result<T>) -> T {
    match result {
        Ok(x) => x,
        Err(e) => {
            e.print().unwrap();
            std::process::exit(1);
        }
    }
}

fn main() -> Result<()> {
    let args: Vec<_> = env::args().collect();
    let file = args.get(1).unwrap();

    let code = std::fs::read_to_string(file).unwrap();

    let tokens = unwrap_or_error(tokenize(&code, file));

    let mut parser = Parser::new(&tokens, file);

    let mut output = File::create("output.asm").unwrap();
    let mut generator = X86NasmGenerator::new(file);

    let mut statements = vec![];

    while !parser.eof() {
        let statement = unwrap_or_error(parser.parse_statement());
        statements.push(statement);
    }

    let mut type_checker = TypeChecker::new(file);

    for statement in &mut statements {
        unwrap_or_error(type_checker.check(statement));
        unwrap_or_error(generator.generate_statement(statement));
    }

    generator.write(&mut output);

    Ok(())
}
