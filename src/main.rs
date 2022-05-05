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

    let source = std::fs::read_to_string(&args.source_path).unwrap();
    let source_file = args.source_path.to_str().unwrap();

    let tokens = unwrap_or_error(tokenize(&source, source_file));

    if args.print_tokens {
        println!("Tokens:");
        println!("{:#?}", tokens);
    }

    let mut parser = Parser::new(&tokens, source_file);

    let assembly_file = &args.assembly_output.unwrap_or(PathBuf::from("output.asm"));

    let mut output = File::create(&assembly_file).unwrap();
    let mut generator = X86NasmGenerator::new(source_file);

    let mut type_checker = TypeChecker::new(source_file);

    while !parser.eof() {
        let statement = unwrap_or_error(parser.parse_statement());
        if args.print_ast {
            println!("Ast:");
            println!("{:#?}", statement);
        }

        let typed_statement = unwrap_or_error(type_checker.transform_statement(statement));
        if args.print_typed_ast {
            println!("Typed ast:");
            println!("{:#?}", typed_statement);
        }

        unwrap_or_error(generator.visit_statement(typed_statement));
    }

    generator.write(&mut output);

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

    Ok(())
}
