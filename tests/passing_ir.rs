use common::run_passing_test;
use dodo::backends::BackendType;
use test_generator::test_resources;

mod common;

#[test_resources("tests/data/*.dodo")]
fn test_ir(path: &str) {
    run_passing_test(path, BackendType::Ir).unwrap();
}
