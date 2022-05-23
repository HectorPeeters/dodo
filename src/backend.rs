use std::path::Path;

use crate::{ast::ConsumingAstVisitor, error::Result, types::Type};

pub trait Backend<RS, RE>: ConsumingAstVisitor<Type, RS, RE> {
    fn finalize(&mut self, output: &Path) -> Result<()>;
}
