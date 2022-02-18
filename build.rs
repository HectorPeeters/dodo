//
// Test case generation code based on
// https://blog.cyplo.net/posts/2018/12/generate-rust-tests-from-data/
//

use std::env;
use std::fs::read_dir;
use std::fs::DirEntry;
use std::fs::File;
use std::io::Write;
use std::path::Path;

// build script's entry point
fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();
    let destination = Path::new(&out_dir).join("tests.rs");
    let mut test_file = File::create(&destination).unwrap();

    // write test file header, put `use`, `const` etc there
    write_header(&mut test_file);

    let test_data_directories = read_dir("./tests/data/").unwrap();

    for directory in test_data_directories {
        write_test(&mut test_file, &directory.unwrap());
    }
}

fn write_test(test_file: &mut File, directory: &DirEntry) {
    let directory = directory.path().canonicalize().unwrap();
    let path = directory.display();
    let test_name = format!(
        "dodo_{}",
        directory
            .file_name()
            .unwrap()
            .to_string_lossy()
            .replace(".dodo", "")
    );

    write!(
        test_file,
        include_str!("./tests/test_template"),
        name = test_name,
        path = path
    )
    .unwrap();
}

fn write_header(test_file: &mut File) {
    write!(
        test_file,
        r#"use dodo::error::Result;
use dodo::parser::Parser;
use dodo::tokenizer::tokenize;
use dodo::type_checker::TypeChecker;
use dodo::x86_nasm::X86NasmGenerator;
use std::collections::hash_map::DefaultHasher;
use std::fs::File;
use std::hash::{{Hash, Hasher}};
use std::process::Command;

fn unwrap_or_error<T>(result: Result<T>) -> T {{
    match result {{
        Ok(x) => x,
        Err(e) => {{
            e.print().unwrap();
            std::process::exit(1);
        }}
    }}
}}

fn run_test(file: &str, code: &str) {{
    let mut hasher = DefaultHasher::new();
    file.hash(&mut hasher);
    let test_code = hasher.finish();

    let tokens = unwrap_or_error(tokenize(&code, file));

    let mut parser = Parser::new(&tokens, file);

    let mut output = File::create(&format!("/tmp/output_{{}}.asm", test_code)).unwrap();
    let mut generator = X86NasmGenerator::new(file);

    let mut statements = vec![];

    while !parser.eof() {{
        let statement = unwrap_or_error(parser.parse_statement());
        statements.push(statement);
    }}

    let mut type_checker = TypeChecker::new(file);

    for statement in &mut statements {{
        unwrap_or_error(type_checker.check(statement));
        unwrap_or_error(generator.generate_statement(statement));
    }}

    generator.write(&mut output);

    Command::new("nasm")
        .args([
            "-f", 
            "elf64", 
            &format!("/tmp/output_{{}}.asm", test_code), 
            "-o", 
            &format!("/tmp/output_{{}}.o", test_code), 
            "-g",
        ])
        .output()
        .expect("Failed to compile assembly");

    Command::new("gcc")
        .args([
            "-o", 
            &format!("/tmp/output_{{}}", test_code), 
            &format!("/tmp/output_{{}}.o", test_code), 
            "-nostartfiles", 
            "-no-pie", 
            "-g",
        ])
        .output()
        .expect("Failed to link");

    Command::new(&format!("/tmp/output_{{}}", test_code))
        .output()
        .expect("Failed to execute");
}}
"#
    )
    .unwrap();
}
