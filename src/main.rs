use clap::StructOpt;
use dodo::ast::{AstTransformer, ConsumingAstVisitor};
use dodo::error::Result;
use dodo::parser::Parser;
use dodo::tokenizer::tokenize;
use dodo::type_checker::TypeChecker;
use dodo::x86_nasm::X86NasmGenerator;
use std::fs::File;
use std::path::PathBuf;
use std::process::Command;
use std::time::Instant;

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
    let args = Args::parse();

    // Reading source

    let mut step_start_time = Instant::now();

    let source = std::fs::read_to_string(&args.source_path).unwrap();
    let source_file = args.source_path.to_str().unwrap();

    let reading_source_duration = step_start_time.elapsed();

    // Tokenizing

    step_start_time = Instant::now();

    let tokens = unwrap_or_error(tokenize(&source, source_file));
    let tokenize_duration = step_start_time.elapsed();

    if args.print_tokens {
        println!("Tokens:");
        println!("{:#?}", tokens);
    }

    // Parsing

    step_start_time = Instant::now();

    let parser = Parser::new(&tokens, source_file);
    let statements = unwrap_or_error(parser.into_iter().collect::<Result<Vec<_>>>());

    let parse_duration = step_start_time.elapsed();

    if args.print_ast {
        println!("Ast:");
        println!("{:#?}", statements);
    }

    // Type checking

    let mut type_checker = TypeChecker::new(source_file);

    step_start_time = Instant::now();

    let statements = statements
        .into_iter()
        .map(|x| type_checker.transform_statement(x))
        .collect::<Result<Vec<_>>>()?;

    let type_check_duration = step_start_time.elapsed();

    if args.print_typed_ast {
        println!("Typed ast:");
        println!("{:#?}", statements);
    }

    // Generating assembly

    step_start_time = Instant::now();

    let assembly_file = &args.assembly_output.unwrap_or(PathBuf::from("output.asm"));

    let mut output = File::create(&assembly_file).unwrap();
    let mut generator = X86NasmGenerator::new(source_file);

    for statement in statements {
        unwrap_or_error(generator.visit_statement(statement));
    }

    generator.write(&mut output);

    let assembly_generation_duration = step_start_time.elapsed();

    // Compiling assemlby

    step_start_time = Instant::now();

    let object_file = std::env::temp_dir().join("output.o");

    Command::new(args.assembler_command)
        .args([
            "-f",
            "elf64",
            assembly_file.to_str().unwrap(),
            "-o",
            object_file.to_str().unwrap(),
            "-g",
        ])
        .output()
        .expect("Failed to execute assembler");

    let compile_duration = step_start_time.elapsed();

    // Linking

    Command::new(args.linker_command)
        .args([
            "-o",
            args.output
                .unwrap_or(PathBuf::from("a.out"))
                .to_str()
                .unwrap(),
            object_file.to_str().unwrap(),
            "-nostartfiles",
            "-no-pie",
            "-g",
        ])
        .output()
        .expect("Failed to execute linker");

    let linking_duration = step_start_time.elapsed();

    if args.include_timings {
        let timings = vec![
            ("Reading source", reading_source_duration),
            ("Tokenizing", tokenize_duration),
            ("Parsing", parse_duration),
            ("Type checking", type_check_duration),
            ("Generating assembly", assembly_generation_duration),
            ("Compiling", compile_duration),
            ("Linking", linking_duration),
        ];

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
