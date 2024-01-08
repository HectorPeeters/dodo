use dodo::{
    backends::{
        c_backend::CBackend, cranelift_backend::CraneliftBackend, ir_backend::IrBackend,
        x86_nasm_backend::X86NasmBackend, Backend, BackendType,
    },
    error::Result,
    optimisations::optimise,
    parser::Parser,
    project::Project,
    tokenizer::tokenize,
    type_checker::TypeChecker,
};
use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    path::{Path, PathBuf},
};
use test_generator::test_resources;

#[test_resources("tests/data/*.dodo")]
fn test_c(path: &str) {
    run_passing_test(path, BackendType::C, false);
}

#[test_resources("tests/data/*.dodo")]
fn test_c_optimised(path: &str) {
    run_passing_test(path, BackendType::C, true);
}

#[test_resources("tests/data/*.dodo")]
fn test_x86(path: &str) {
    run_passing_test(path, BackendType::X86, false);
}

#[test_resources("tests/data/*.dodo")]
fn test_x86_optimised(path: &str) {
    run_passing_test(path, BackendType::X86, true);
}

#[test_resources("tests/data/*.dodo")]
fn test_cranelift(path: &str) {
    run_passing_test(path, BackendType::Cranelift, false);
}

#[test_resources("tests/data/*.dodo")]
fn test_cranelift_optimised(path: &str) {
    run_passing_test(path, BackendType::Cranelift, true);
}

#[test_resources("tests/failing/*.dodo")]
fn test_c_failing(path: &str) {
    run_failing_test(path, BackendType::C);
}

#[test_resources("tests/failing/*.dodo")]
fn test_x86_failing(path: &str) {
    run_failing_test(path, BackendType::X86);
}

#[test_resources("tests/failing/*.dodo")]
fn test_cranelift_failing(path: &str) {
    run_failing_test(path, BackendType::Cranelift);
}

pub fn run_passing_test(file: &str, backend_type: BackendType, enable_optimization: bool) {
    let source = std::fs::read_to_string(file).unwrap();
    let expected = std::fs::read_to_string(PathBuf::from(file).with_extension("txt")).unwrap();

    let output = run_test(file, &source, backend_type, enable_optimization).unwrap();
    assert_eq!(output.trim(), expected.trim());
}

pub fn run_failing_test(file: &str, backend_type: BackendType) {
    let source = std::fs::read_to_string(file).unwrap();

    let expected = source
        .lines()
        .find(|x| x.starts_with("// Expected: "))
        .map(|x| x.replace("// Expected: ", ""));

    let result = run_test(file, &source, backend_type, false);
    assert!(result.is_err());

    if let Some(expected) = expected {
        assert_eq!(result.unwrap_err().message.trim(), expected.trim());
    }
}

fn run_test(
    file: &str,
    source: &str,
    backend_type: BackendType,
    enable_optimization: bool,
) -> Result<String> {
    println!("RUNNING '{file}' with backend '{backend_type:?}'...");

    let mut hasher = DefaultHasher::new();
    file.hash(&mut hasher);
    enable_optimization.hash(&mut hasher);
    let test_code = hasher.finish();

    let tokens = tokenize(source)?;

    let parser = Parser::new(&tokens);
    let statements = parser.into_iter().collect::<Result<Vec<_>>>()?;

    let mut project = Project::new(file);

    let mut type_checker = TypeChecker::new(&mut project);
    let mut statements = statements
        .into_iter()
        .map(|x| type_checker.check_upper_statement(x))
        .collect::<Result<Vec<_>>>()?;

    if enable_optimization {
        statements = optimise(statements, &project, true);
    }

    let mut backend: Box<dyn Backend> = match backend_type {
        BackendType::C => Box::new(CBackend::new(&mut project)),
        BackendType::X86 => Box::new(X86NasmBackend::new(&mut project)),
        BackendType::Ir => Box::new(IrBackend::new(&mut project)),
        BackendType::Cranelift => Box::new(CraneliftBackend::new(&mut project)),
    };

    backend.process_upper_statements(statements)?;

    let executable_path = format!("/tmp/output_{test_code}");
    let executable_path = Path::new(&executable_path);
    backend.finalize(executable_path, false)?;

    let output = backend.run(executable_path)?;

    Ok(output)
}
