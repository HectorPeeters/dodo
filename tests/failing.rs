use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    path::Path,
};

use dodo::{
    backend::{Backend, BackendType},
    cpp::CppGenerator,
    error::Result,
    parser::Parser,
    project::Project,
    tokenizer::tokenize,
    type_checker::TypeChecker,
    x86_nasm::X86NasmGenerator,
};

use test_case::test_case;

fn run_normal_test(file: &str, backend_type: BackendType) -> Result<()> {
    println!("RUNNING '{}' with backend '{:?}'...", file, backend_type);

    let mut hasher = DefaultHasher::new();
    file.hash(&mut hasher);
    let test_code = hasher.finish();

    let code = std::fs::read_to_string(file).unwrap();

    let tokens = tokenize(&code)?;

    let parser = Parser::new(&tokens);
    let statements = parser.into_iter().collect::<Result<Vec<_>>>()?;

    let mut project = Project::new(file);

    let mut type_checker = TypeChecker::new(&mut project);
    let statements = statements
        .into_iter()
        .map(|x| type_checker.check_upper_statement(x))
        .collect::<Result<Vec<_>>>()?;

    let mut backend: Box<dyn Backend> = match backend_type {
        BackendType::Cpp => Box::new(CppGenerator::new(&mut project)),
        BackendType::X86 => Box::new(X86NasmGenerator::new(&mut project)),
    };

    statements
        .into_iter()
        .map(|x| backend.process_upper_statement(x))
        .collect::<Result<Vec<_>>>()?;

    let executable_path = format!("/tmp/output_{}", test_code);
    let executable_path = Path::new(&executable_path);
    backend.finalize(executable_path)?;

    Ok(())
}

#[test_case("tests/failing/assign_incompatible.dodo"; "assign incompatible")]
#[test_case("tests/failing/assign_return_type_incompatible.dodo"; "assign return type incompatible")]
#[test_case("tests/failing/function_parameter_incompatible.dodo"; "function parameter incompatible")]
#[test_case("tests/failing/if_condition_not_bool.dodo"; "if condition not bool")]
#[test_case("tests/failing/invalid_infix.dodo"; "invalid infix")]
#[test_case("tests/failing/invalid_literal.dodo"; "invalid literal")]
#[test_case("tests/failing/invalid_prefix.dodo"; "invalid prefix")]
#[test_case("tests/failing/invalid_statement.dodo"; "invalid statement")]
#[test_case("tests/failing/invalid_type.dodo"; "invalid type")]
#[test_case("tests/failing/modulo_eight_bit.dodo"; "modulo eight bit")]
#[test_case("tests/failing/unexpected_token.dodo"; "unexpected token")]
#[test_case("tests/failing/unfinished_token_stream.dodo"; "unfinished token stream")]
#[test_case("tests/failing/unfinished_token_stream_2.dodo"; "unfinished token stream 2")]
#[test_case("tests/failing/while_condition_not_bool.dodo"; "while condition not bool")]
fn test_for_all_backends(path: &str) -> Result<()> {
    let backend_types = vec![BackendType::Cpp, BackendType::X86];

    for b in backend_types {
        assert!(run_normal_test(path, b).is_err());
    }

    Ok(())
}
