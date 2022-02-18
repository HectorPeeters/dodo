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

fn main() -> Result<(), std::io::Error> {
    let out_dir = env::var("OUT_DIR").unwrap();
    let destination = Path::new(&out_dir).join("tests.rs");
    let mut test_file = File::create(&destination).unwrap();

    // write test file header, this includes imports and a run function
    write!(test_file, include_str!("./tests/test_header"),)?;

    for directory in read_dir("./tests/data/")? {
        write_test(&mut test_file, &directory?)?;
    }

    Ok(())
}

fn write_test(test_file: &mut File, directory: &DirEntry) -> Result<(), std::io::Error> {
    let directory = directory.path().canonicalize()?;
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
    )?;

    Ok(())
}
