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

fn run_normal_test(file: &str, backend: &mut dyn Backend) -> Result<()> {
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
        .map(|x| type_checker.transform_statement(x))
        .collect::<Result<Vec<_>>>()?;

    statements
        .into_iter()
        .map(|x| backend.process_statement(x))
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

#[test]
fn test_args() -> Result<()> {
    test_for_all_backends("tests/data/args.dodo")
}

#[test]
fn test_comparisons() -> Result<()> {
    test_for_all_backends("tests/data/comparisons.dodo")
}

#[test]
fn test_fibonacci() -> Result<()> {
    test_for_all_backends("tests/data/fibonacci.dodo")
}

#[test]
fn test_fibonacci_recursive() -> Result<()> {
    test_for_all_backends("tests/data/fibonacci_recursive.dodo")
}

#[test]
fn test_math() -> Result<()> {
    test_for_all_backends("tests/data/math.dodo")
}

#[test]
fn test_nested() -> Result<()> {
    test_for_all_backends("tests/data/nested.dodo")
}

#[test]
fn test_pointers() -> Result<()> {
    test_for_all_backends("tests/data/pointers.dodo")
}

#[test]
fn test_primes() -> Result<()> {
    test_for_all_backends("tests/data/primes.dodo")
}

#[test]
fn test_print() -> Result<()> {
    test_for_all_backends("tests/data/print.dodo")
}

#[test]
fn test_pythagorean_triplets() -> Result<()> {
    test_for_all_backends("tests/data/pythagorean_triplets.dodo")
}

#[test]
fn test_return() -> Result<()> {
    test_for_all_backends("tests/data/return.dodo")
}

#[test]
fn test_scopes() -> Result<()> {
    test_for_all_backends("tests/data/scopes.dodo")
}

#[test]
fn test_string() -> Result<()> {
    test_for_all_backends("tests/data/string.dodo")
}

#[test]
fn test_types() -> Result<()> {
    test_for_all_backends("tests/data/types.dodo")
}
