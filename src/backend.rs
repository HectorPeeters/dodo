use std::path::Path;

use crate::{ast::TypedStatement, error::Result};

pub trait Backend {
    fn process_statement(&mut self, statement: TypedStatement) -> Result<()>;

    fn finalize(&mut self, output: &Path) -> Result<()>;

    fn name(&self) -> &'static str;
}
