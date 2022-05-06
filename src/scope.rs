use crate::{error::*, tokenizer::SourceRange};
use std::collections::HashMap;

pub struct Scope<'a, T> {
    items: Vec<HashMap<String, T>>,
    source_file: &'a str,
}

impl<'a, T: Clone> Scope<'a, T> {
    pub fn new(source_file: &'a str) -> Self {
        Self {
            items: vec![HashMap::new()],
            source_file,
        }
    }

    pub fn insert(&mut self, name: &str, data: T, range: &SourceRange) -> Result<()> {
        match self.items.last_mut() {
            Some(last_scope) => {
                if last_scope.contains_key(name) {
                    Err(Error::new(
                        ErrorType::Scope,
                        format!("Identifier '{}' already defined in scope", name),
                        *range,
                        self.source_file.to_string(),
                    ))
                } else {
                    last_scope.insert(name.to_owned(), data);
                    Ok(())
                }
            }
            None => Err(Error::new(
                ErrorType::Scope,
                format!("Trying to insert '{}' into empty scope", name),
                *range,
                self.source_file.to_string(),
            )),
        }
    }

    pub fn push(&mut self) {
        self.items.push(HashMap::new());
    }

    pub fn pop(&mut self, range: &SourceRange) -> Result<()> {
        self.items
            .pop()
            .ok_or_else(|| {
                Error::new(
                    ErrorType::Scope,
                    "Tried popping while scope stack was empty".to_string(),
                    *range,
                    self.source_file.to_string(),
                )
            })
            .map(|_| ())
    }

    pub fn find(&self, name: &str, range: &SourceRange) -> Result<T> {
        self.items
            .iter()
            .rev()
            .find_map(|x| x.get(name))
            .cloned()
            .ok_or_else(|| {
                Error::new(
                    ErrorType::Scope,
                    format!("'{}' not found in scope", name),
                    *range,
                    self.source_file.to_string(),
                )
            })
    }

    pub fn size(&self) -> Result<usize> {
        Ok(self.items.iter().map(|x| x.len()).sum())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scope_simple() -> Result<()> {
        let mut scope: Scope<u32> = Scope::new("test.dodo");
        scope.push();
        scope.insert("test", 12, &(0..0).into())?;
        assert_eq!(scope.find("test", &(0..0).into())?, 12);
        scope.pop(&(0..0).into())?;
        Ok(())
    }

    #[test]
    fn scope_multiple() -> Result<()> {
        let mut scope: Scope<u32> = Scope::new("test.dodo");
        scope.push();
        scope.insert("test", 12, &(0..0).into())?;
        scope.insert("tast", 13, &(0..0).into())?;
        assert_eq!(scope.find("test", &(0..0).into())?, 12);
        assert_eq!(scope.find("tast", &(0..0).into())?, 13);
        scope.pop(&(0..0).into())?;
        Ok(())
    }

    #[test]
    fn scope_nested() -> Result<()> {
        let mut scope: Scope<u32> = Scope::new("test.dodo");
        scope.push();
        scope.insert("test", 12, &(0..0).into())?;
        scope.push();
        scope.insert("test", 13, &(0..0).into())?;
        assert_eq!(scope.find("test", &(0..0).into())?, 13);
        scope.pop(&(0..0).into())?;
        assert_eq!(scope.find("test", &(0..0).into())?, 12);
        scope.pop(&(0..0).into())?;
        Ok(())
    }

    #[test]
    fn scope_key_already_defined() -> Result<()> {
        let mut scope: Scope<u32> = Scope::new("test.dodo");
        scope.insert("test", 12, &(0..0).into())?;

        assert!(scope.insert("test", 13, &(0..0).into()).is_err());

        Ok(())
    }

    #[test]
    fn scope_pop_empty() -> Result<()> {
        let mut scope: Scope<u32> = Scope::new("test.dodo");

        scope.pop(&(0..0).into())?;
        assert!(scope.pop(&(0..0).into()).is_err());

        Ok(())
    }

    #[test]
    fn scope_insert_empty() -> Result<()> {
        let mut scope: Scope<u32> = Scope::new("test.dodo");

        scope.pop(&(0..0).into())?;
        assert!(scope.insert("test", 12, &(0..0).into()).is_err());

        Ok(())
    }

    #[test]
    fn scope_find_non_existent() -> Result<()> {
        let scope: Scope<u32> = Scope::new("test.dodo");

        assert!(scope.find("test", &(0..0).into()).is_err());

        Ok(())
    }
}
