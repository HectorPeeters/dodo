use common::run_failing_test;
use dodo::backends::BackendType;
use test_generator::test_resources;

mod common;

#[test_resources("tests/failing/*.dodo")]
fn test_x86(path: &str) {
    run_failing_test(path, BackendType::X86);
}
