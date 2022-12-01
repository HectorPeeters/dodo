use clap::StructOpt;
use dodo::backends::BackendType;
use dodo::backends::{
    c_backend::CBackend, ir_backend::IrBackend, x86_nasm_backend::X86NasmBackend,
};
use dodo::error::Result;
use dodo::parser::Parser;
use dodo::tokenizer::tokenize;
use dodo::type_checker::TypeChecker;
use dodo::{backends::*, project::Project};
use std::path::PathBuf;

#[derive(clap::Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    source_path: std::path::PathBuf,
    #[clap(short, long)]
    output: Option<std::path::PathBuf>,

    #[clap(short, long, arg_enum)]
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
fn main() -> Result<()> {
    use dodo::optimisations::optimise;

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

    let mut project = Project::new(source_file);

    let mut type_checker = TypeChecker::new(&mut project);
    let mut statements = unwrap_or_error(
        statements
            .into_iter()
            .map(|x| type_checker.check_upper_statement(x))
            .collect::<Result<Vec<_>>>(),
        source_file,
    );

    if args.print_typed_ast {
        println!("Typed ast:");
        println!("{:#?}", statements);
    }

    // Optimisation

    if args.optimise {
        statements = optimise(statements, &project, args.print_optimisations);
    }

    if args.print_optimised_ast {
        println!("Optimised ast:");
        println!("{:#?}", statements);
    }

    // Backend

    let mut backend: Box<dyn Backend> = match args.backend {
        Some(BackendType::C) | None => Box::new(CBackend::new(&mut project)),
        Some(BackendType::X86) => Box::new(X86NasmBackend::new(&mut project)),
        Some(BackendType::Ir) => Box::new(IrBackend::new(&mut project)),
    };

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
        println!("{}", output);
    }

    Ok(())
}
