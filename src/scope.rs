use crate::error::*;
use std::collections::HashMap;

pub struct Scope<T> {
    items: Vec<HashMap<String, T>>,
    // TODO: convert this to a reference
    source_file: String,
}

impl<T: Copy> Scope<T> {
    pub fn new(source_file: &str) -> Self {
        Self {
            items: vec![],
            source_file: source_file.to_string(),
        }
    }

    pub fn insert(&mut self, name: &str, data: T) -> Result<()> {
        match self.items.last_mut() {
            Some(last_scope) => {
                if last_scope.contains_key(name) {
                    Err(Error::new(
                        ErrorType::Scope,
                        format!("Identifier '{}' already defined in scope", name),
                        0..0,
                        self.source_file.clone(),
                    ))
                } else {
                    last_scope.insert(name.to_owned(), data);
                    Ok(())
                }
            }
            None => Err(Error::new(
                ErrorType::Scope,
                format!("Trying to insert '{}' into empty scope", name),
                0..0,
                self.source_file.clone(),
            )),
        }
    }

    pub fn push(&mut self) {
        self.items.push(HashMap::new());
    }

    pub fn pop(&mut self) -> Result<()> {
        self.items
            .pop()
            .ok_or_else(|| {
                Error::new(
                    ErrorType::Scope,
                    format!("Tried popping while scope stack was empty"),
                    0..0,
                    "".to_string(),
                )
            })
            .map(|_| ())
    }

    pub fn find(&self, name: &str) -> Result<T> {
        self.items
            .iter()
            .rev()
            .find_map(|x| x.get(name).copied())
            .ok_or_else(|| {
                Error::new(
                    ErrorType::Scope,
                    format!("'{}' not found in scope", name),
                    0..0,
                    "".to_string(),
                )
            })
    }

    pub fn len(&self) -> Result<usize> {
        Ok(self.items.iter().map(|x| x.len()).sum())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scope_simple() -> Result<()> {
        let mut scope: Scope<u32> = Scope::new();
        scope.push();
        scope.insert("test", 12)?;
        assert_eq!(scope.find("test")?, 12);
        scope.pop()?;
        Ok(())
    }

    #[test]
    fn scope_multiple() -> Result<()> {
        let mut scope: Scope<u32> = Scope::new();
        scope.push();
        scope.insert("test", 12)?;
        scope.insert("tast", 13)?;
        assert_eq!(scope.find("test")?, 12);
        assert_eq!(scope.find("tast")?, 13);
        scope.pop()?;
        Ok(())
    }

    #[test]
    fn scope_nested() -> Result<()> {
        let mut scope: Scope<u32> = Scope::new();
        scope.push();
        scope.insert("test", 12)?;
        scope.push();
        scope.insert("test", 13)?;
        assert_eq!(scope.find("test")?, 13);
        scope.pop()?;
        assert_eq!(scope.find("test")?, 12);
        scope.pop()?;
        Ok(())
    }
}
