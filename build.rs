//
// Test case generation code based on
// https://blog.cyplo.net/posts/2018/12/generate-rust-tests-from-data/
//

use std::env;
use std::ffi::OsStr;
use std::fs::read_dir;
use std::fs::DirEntry;
use std::fs::File;
use std::io::Write;
use std::path::Path;

fn main() -> Result<(), std::io::Error> {
    let out_dir = env::var("OUT_DIR").unwrap();

    // Passing tests

    let destination = Path::new(&out_dir).join("tests.rs");
    let mut test_output = File::create(&destination).unwrap();

    // write test file header, this includes imports and a run function
    write!(test_output, include_str!("./tests/test_header"),)?;

    for test_file in read_dir("./tests/data/")? {
        let test_file = test_file?;
        if test_file.path().extension() == Some(OsStr::new("dodo")) {
            write_test(&mut test_output, &test_file, false)?;
        }
    }

    // Failing tests

    for test_file in read_dir("./tests/failing/")? {
        let test_file = test_file?;
        if test_file.path().extension() == Some(OsStr::new("dodo")) {
            write_test(&mut test_output, &test_file, true)?;
        }
    }

    Ok(())
}

fn write_test(
    test_output: &mut File,
    test_file: &DirEntry,
    is_failing: bool,
) -> Result<(), std::io::Error> {
    let test_file = test_file.path().canonicalize()?;
    let path = test_file.display();
    let test_name = format!(
        "dodo_{}_{}",
        if is_failing { "failing" } else { "passing" },
        test_file
            .file_name()
            .unwrap()
            .to_string_lossy()
            .replace(".dodo", "")
    );

    let expected = if !is_failing {
        let expected_path = test_file.with_extension("txt");
        if !expected_path.exists() {
            panic!("No expected output file present for test {:?}", test_file);
        }
        std::fs::read_to_string(expected_path)?
    } else {
        "".to_string()
    };

    write!(
        test_output,
        include_str!("./tests/test_template"),
        name = test_name,
        path = path,
        expected = expected,
        is_failing = is_failing,
    )?;

    Ok(())
}
