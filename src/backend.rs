use std::path::Path;

use crate::{ast::TypedUpperStatement, error::Result};

pub trait Backend {
    fn process_upper_statement(&mut self, statement: TypedUpperStatement) -> Result<()>;

    fn finalize(&mut self, output: &Path) -> Result<()>;

    fn name(&self) -> &'static str;
}
