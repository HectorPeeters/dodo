use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    path::{Path, PathBuf},
    process::Command,
};

use dodo::{
    ast::AstTransformer, backend::Backend, cpp::CppGenerator, error::Result, parser::Parser,
    tokenizer::tokenize, type_checker::TypeChecker, x86_nasm::X86NasmGenerator,
};

use test_case::test_case;

fn run_normal_test(file: &str, backend: &mut dyn Backend) -> Result<()> {
    println!("RUNNING '{}' with backend '{}'...", file, backend.name());

    let mut hasher = DefaultHasher::new();
    file.hash(&mut hasher);
    let test_code = hasher.finish();

    let code = std::fs::read_to_string(file).unwrap();
    let expected = std::fs::read_to_string(PathBuf::from(file).with_extension("txt")).unwrap();

    let tokens = tokenize(&code)?;

    let parser = Parser::new(&tokens);
    let statements = parser.into_iter().collect::<Result<Vec<_>>>()?;

    let mut type_checker = TypeChecker::new();
    let statements = statements
        .into_iter()
        .map(|x| type_checker.transform_upper_statement(x))
        .collect::<Result<Vec<_>>>()?;

    statements
        .into_iter()
        .map(|x| backend.process_upper_statement(x))
        .collect::<Result<Vec<_>>>()?;

    let executable_path = format!("/tmp/output_{}", test_code);
    let executable_path = Path::new(&executable_path);
    backend.finalize(&executable_path)?;

    let output = Command::new(executable_path)
        .output()
        .expect("Failed to execute");

    assert_eq!(
        String::from_utf8_lossy(&output.stdout).trim(),
        expected.trim()
    );

    Ok(())
}

#[test_case("tests/data/args.dodo"; "args")]
#[test_case("tests/data/comparisons.dodo"; "commparisons")]
#[test_case("tests/data/fibonacci.dodo"; "fibonacci")]
#[test_case("tests/data/fibonacci_recursive.dodo"; "fibonacci recursive")]
#[test_case("tests/data/math.dodo"; "math")]
#[test_case("tests/data/nested.dodo"; "nested")]
#[test_case("tests/data/pointers.dodo"; "pointers")]
#[test_case("tests/data/primes.dodo"; "primes")]
#[test_case("tests/data/print.dodo"; "print")]
#[test_case("tests/data/pythagorean_triplets.dodo"; "pythagorean triplets")]
#[test_case("tests/data/return.dodo"; "returns")]
#[test_case("tests/data/scopes.dodo"; "scopes")]
#[test_case("tests/data/section_annotation.dodo"; "annotations")]
#[test_case("tests/data/string.dodo"; "string")]
#[test_case("tests/data/types.dodo"; "types")]
fn test_for_all_backends(path: &str) -> Result<()> {
    let backends: Vec<Box<dyn Backend>> = vec![
        Box::new(X86NasmGenerator::new()),
        Box::new(CppGenerator::new()),
    ];

    for mut b in backends {
        run_normal_test(path, &mut *b)?;
    }

    Ok(())
}
