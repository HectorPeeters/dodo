#![feature(exit_status_error)]

use clap::StructOpt;
use dodo::ast::{AstTransformer, ConsumingAstVisitor};
use dodo::error::Result;
use dodo::parser::Parser;
use dodo::tokenizer::tokenize;
use dodo::type_checker::TypeChecker;
use std::time::{Duration, Instant};

#[derive(clap::Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    source_path: std::path::PathBuf,
    #[clap(short, long)]
    output: Option<std::path::PathBuf>,
    #[clap(long)]
    assembly_output: Option<std::path::PathBuf>,

    #[clap(short, long, default_value_t = String::from("nasm"))]
    assembler_command: String,
    #[clap(short, long, default_value_t = String::from("gcc"))]
    linker_command: String,

    #[clap(long)]
    print_ast: bool,
    #[clap(long)]
    print_typed_ast: bool,
    #[clap(long)]
    print_tokens: bool,

    #[clap(long)]
    include_timings: bool,
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
    use dodo::ir::IrBuilder;

    let args = Args::parse();

    let mut timings: Vec<(&'static str, Duration)> = vec![];

    // Reading source

    let step_start_time = Instant::now();

    let source = std::fs::read_to_string(&args.source_path);
    if source.is_err() {
        println!("Failed to read input file {:?}", args.source_path);
        return Ok(());
    }
    let source = source.unwrap();

    let source_file = args.source_path.to_str().unwrap();

    timings.push(("Reading source", step_start_time.elapsed()));

    // Tokenizing

    let step_start_time = Instant::now();
    let tokens = unwrap_or_error(tokenize(&source), source_file);
    timings.push(("Tokenizing", step_start_time.elapsed()));

    if args.print_tokens {
        println!("Tokens:");
        println!("{:#?}", tokens);
    }

    // Parsing

    let step_start_time = Instant::now();
    let parser = Parser::new(&tokens);
    let statements = unwrap_or_error(parser.into_iter().collect::<Result<Vec<_>>>(), source_file);
    timings.push(("Parsing", step_start_time.elapsed()));

    if args.print_ast {
        println!("Ast:");
        println!("{:#?}", statements);
    }

    // Type checking

    let step_start_time = Instant::now();
    let mut type_checker = TypeChecker::new();
    let statements = unwrap_or_error(
        statements
            .into_iter()
            .map(|x| type_checker.transform_statement(x))
            .collect::<Result<Vec<_>>>(),
        source_file,
    );
    timings.push(("Type checking", step_start_time.elapsed()));

    if args.print_typed_ast {
        println!("Typed ast:");
        println!("{:#?}", statements);
    }

    // Generating ir

    let mut ir_builder = IrBuilder::new();
    for stmt in statements {
        ir_builder.visit_statement(stmt)?;
    }
    ir_builder.print_blocks();

    // Reporting

    if args.include_timings {
        for (name, duration) in &timings {
            println!("{}: {} ms", name, duration.as_micros() as f64 / 1000.0);
        }

        println!(
            "\nTotal: {} ms",
            timings
                .into_iter()
                .map(|(_, duration)| duration.as_millis())
                .sum::<u128>()
        );
    }

    Ok(())
}
