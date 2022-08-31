use clap::ArgEnum;
use clap::StructOpt;
use dodo::ast::AstTransformer;
use dodo::backend::Backend;
use dodo::cpp::CppGenerator;
use dodo::error::Result;
use dodo::parser::Parser;
use dodo::tokenizer::tokenize;
use dodo::type_checker::TypeChecker;
use dodo::x86_nasm::X86NasmGenerator;
use std::path::PathBuf;

#[derive(Debug, Clone, Copy, ArgEnum)]
pub enum Backends {
    X86,
    Cpp,
}

#[derive(clap::Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    source_path: std::path::PathBuf,
    #[clap(short, long)]
    output: Option<std::path::PathBuf>,

    #[clap(short, long, arg_enum)]
    backend: Option<Backends>,

    #[clap(long)]
    print_tokens: bool,
    #[clap(long)]
    print_ast: bool,
    #[clap(long)]
    print_typed_ast: bool,
}

#[cfg(not(tarpaulin_include))]
fn unwrap_or_error<T>(result: Result<T>, source_file: &str) -> T {
    match result {
        Ok(x) => x,
        Err(e) => {
            e.print(source_file).unwrap();
            std::process::exit(1);
        }
    }
}

#[cfg(not(tarpaulin_include))]
fn main() -> Result<()> {
    let args = Args::parse();

    // Reading source

    let source = std::fs::read_to_string(&args.source_path);
    if source.is_err() {
        println!("Failed to read input file {:?}", args.source_path);
        return Ok(());
    }
    let source = source.unwrap();

    let source_file = args.source_path.to_str().unwrap();

    // Tokenizing

    let tokens = unwrap_or_error(tokenize(&source), source_file);

    if args.print_tokens {
        println!("Tokens:");
        println!("{:#?}", tokens);
    }

    // Parsing

    let parser = Parser::new(&tokens);
    let statements = unwrap_or_error(parser.into_iter().collect::<Result<Vec<_>>>(), source_file);

    if args.print_ast {
        println!("Ast:");
        println!("{:#?}", statements);
    }

    // Type checking

    let mut type_checker = TypeChecker::new();
    let statements = unwrap_or_error(
        statements
            .into_iter()
            .map(|x| type_checker.transform_upper_statement(x))
            .collect::<Result<Vec<_>>>(),
        source_file,
    );

    if args.print_typed_ast {
        println!("Typed ast:");
        println!("{:#?}", statements);
    }

    // Backend

    let mut backend: Box<dyn Backend> = match args.backend {
        Some(Backends::X86) | None => Box::new(X86NasmGenerator::new()),
        Some(Backends::Cpp) => Box::new(CppGenerator::new()),
    };

    unwrap_or_error(
        statements
            .into_iter()
            .map(|x| backend.process_upper_statement(x))
            .collect::<Result<Vec<_>>>(),
        source_file,
    );

    backend.finalize(&args.output.unwrap_or_else(|| PathBuf::from("a.out")))?;

    Ok(())
}
