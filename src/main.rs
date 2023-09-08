use std::path::PathBuf;

use dodo::backends::BackendType;
use dodo::backends::{c_backend::CBackend, x86_nasm_backend::X86NasmBackend, Backend};
use dodo::error::Result;
use dodo::lexer::lex;
use dodo::parser::Parser;
use dodo::sema::Sema;

#[derive(clap::Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    source_path: std::path::PathBuf,
    #[clap(short, long)]
    output: Option<std::path::PathBuf>,

    #[clap(short, long)]
    backend: Option<BackendType>,

    #[clap(long)]
    print_tokens: bool,
    #[clap(long)]
    print_ast: bool,
    #[clap(long)]
    print_typed_ast: bool,
    #[clap(long)]
    print_optimised_ast: bool,
    #[clap(long)]
    print_optimisations: bool,

    #[clap(short, long)]
    run: bool,

    #[clap(short = 'O', long)]
    optimise: bool,

    #[clap(long)]
    dont_compile: bool,
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
fn get_args() -> Args {
    use clap::Parser;
    Args::parse()
}

#[cfg(not(tarpaulin_include))]
fn main() -> Result<()> {
    // Reading source
    let args = get_args();

    let source = std::fs::read_to_string(&args.source_path);
    if source.is_err() {
        println!("Failed to read input file {:?}", args.source_path);
        return Ok(());
    }
    let source = source.unwrap();

    let source_file = args.source_path.to_str().unwrap();

    // Tokenizing

    let tokens = unwrap_or_error(lex(&source), source_file);

    if args.print_tokens {
        println!("Tokens:");
        println!("{tokens:#?}");
    }

    // Parsing

    let parser = Parser::new(&tokens);
    let statements = unwrap_or_error(parser.into_iter().collect::<Result<Vec<_>>>(), source_file);

    if args.print_ast {
        println!("Ast:");
        println!("{statements:#?}");
    }

    // Semantic analysis

    let mut sema = Sema::new();
    let statements = unwrap_or_error(sema.analyse(statements), source_file);

    if args.print_typed_ast {
        println!("Typed ast:");
        println!("{statements:#?}");
    }

    // Backend

    let mut backend: Box<dyn Backend> = match args.backend {
        Some(BackendType::C) | None => Box::new(CBackend::new(&sema)),
        Some(BackendType::X86) => Box::new(X86NasmBackend::new(&sema)),
    };

    backend.prepare(&statements)?;

    unwrap_or_error(
        statements
            .into_iter()
            .map(|x| backend.process_upper_statement(x))
            .collect::<Result<Vec<_>>>(),
        source_file,
    );

    let output_executable = args.output.unwrap_or_else(|| PathBuf::from("a.out"));
    backend.finalize(&output_executable, args.dont_compile)?;

    if args.run {
        let output = backend.run(&output_executable)?;
        println!("{output}");
    }

    Ok(())
}
