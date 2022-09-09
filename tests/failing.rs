use dodo::{
    backends::{
        c_backend::CBackend, ir_backend::IrBackend, x86_nasm_backend::X86NasmBackend, Backend,
        BackendType,
    },
    error::Result,
    parser::Parser,
    project::Project,
    tokenizer::tokenize,
    type_checker::TypeChecker,
};
use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    path::Path,
};
use test_generator::test_resources;

fn run_test(file: &str, source: &str, backend_type: BackendType) -> Result<()> {
    println!("RUNNING '{}' with backend '{:?}'...", file, backend_type);

    let mut hasher = DefaultHasher::new();
    file.hash(&mut hasher);
    let test_code = hasher.finish();

    let tokens = tokenize(source)?;

    let parser = Parser::new(&tokens);
    let statements = parser.into_iter().collect::<Result<Vec<_>>>()?;

    let mut project = Project::new(file);

    let mut type_checker = TypeChecker::new(&mut project);
    let statements = statements
        .into_iter()
        .map(|x| type_checker.check_upper_statement(x))
        .collect::<Result<Vec<_>>>()?;

    let mut backend: Box<dyn Backend> = match backend_type {
        BackendType::C => Box::new(CBackend::new(&mut project)),
        BackendType::X86 => Box::new(X86NasmBackend::new(&mut project)),
        BackendType::Ir => Box::new(IrBackend::new(&mut project)),
    };

    statements
        .into_iter()
        .map(|x| backend.process_upper_statement(x))
        .collect::<Result<Vec<_>>>()?;

    let executable_path = format!("/tmp/output_{}", test_code);
    let executable_path = Path::new(&executable_path);
    backend.finalize(executable_path, false)?;

    Ok(())
}

#[test_resources("tests/failing/*.dodo")]
fn test_for_all_backends(path: &str) {
    let backend_types = vec![BackendType::C, BackendType::X86];

    for b in backend_types {
        let code = std::fs::read_to_string(path).unwrap();

        let expected = code
            .lines()
            .find(|x| x.starts_with("// Expected: "))
            .map(|x| x.replace("// Expected: ", ""));

        let result = run_test(path, &code, b);
        assert!(result.is_err());

        if let Some(expected) = expected {
            assert_eq!(result.unwrap_err().message.trim(), expected.trim());
        }
    }
}
