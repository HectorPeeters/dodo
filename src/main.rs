use dodo::error::Result;
use dodo::parser::Parser;
use dodo::tokenizer::tokenize;
use dodo::type_checker::TypeChecker;
use dodo::x86_nasm::X86NasmGenerator;
use std::env;
use std::fs::File;

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
        unwrap_or_error(type_checker.check(statement, None));
        unwrap_or_error(generator.generate_statement(statement));
    }

    generator.write(&mut output);

    Ok(())
}
