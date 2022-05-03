use dodo::ast::AstTransformer;
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

    let source = std::fs::read_to_string(file).unwrap();

    let tokens = unwrap_or_error(tokenize(&source, file));

    let mut parser = Parser::new(&tokens, file);

    let mut output = File::create("output.asm").unwrap();
    let mut generator = X86NasmGenerator::new(file);

    let mut type_checker = TypeChecker::new(file);

    while !parser.eof() {
        let statement = unwrap_or_error(parser.parse_statement());
        let typed_statement = unwrap_or_error(type_checker.transform_statement(statement));

        unwrap_or_error(generator.generate_statement(typed_statement));
    }

    generator.write(&mut output);

    Ok(())
}
