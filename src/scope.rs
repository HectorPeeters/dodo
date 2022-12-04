use crate::error::*;
use std::collections::HashMap;

#[derive(Default)]
pub struct Scope<T> {
    items: Vec<HashMap<String, T>>,
}

impl<T: Clone> Scope<T> {
    pub fn new() -> Self {
        Self {
            items: vec![HashMap::new()],
        }
    }

    pub fn insert(&mut self, name: &str, data: T) -> Result<()> {
        match self.items.last_mut() {
            Some(last_scope) => {
                if last_scope.contains_key(name) {
                    Err(Error::new(
                        ErrorType::Scope,
                        format!("Identifier '{name}' already defined in scope"),
                    ))
                } else {
                    last_scope.insert(name.to_owned(), data);
                    Ok(())
                }
            }
            None => Err(Error::new(
                ErrorType::Scope,
                format!("Trying to insert '{name}' into empty scope"),
            )),
        }
    }

    pub fn push(&mut self) {
        self.items.push(HashMap::new());
    }

    pub fn pop(&mut self) {
        self.items
            .pop()
            .expect("Tried popping while scope stack was empty");
    }

    pub fn find(&self, name: &str) -> Result<T> {
        self.items
            .iter()
            .rev()
            .find_map(|x| x.get(name))
            .cloned()
            .ok_or_else(|| Error::new(ErrorType::Scope, format!("'{name}' not found in scope")))
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
        let mut scope: Scope<u32> = Scope::new();
        scope.push();
        scope.insert("test", 12)?;
        assert_eq!(scope.find("test")?, 12);
        scope.pop();
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
        scope.pop();
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
        scope.pop();
        assert_eq!(scope.find("test")?, 12);
        scope.pop();
        Ok(())
    }

    #[test]
    fn scope_key_already_defined() -> Result<()> {
        let mut scope: Scope<u32> = Scope::new();
        scope.insert("test", 12)?;

        assert!(scope.insert("test", 13).is_err());

        Ok(())
    }

    #[test]
    fn scope_insert_empty() -> Result<()> {
        let mut scope: Scope<u32> = Scope::new();

        scope.pop();
        assert!(scope.insert("test", 12).is_err());

        Ok(())
    }

    #[test]
    fn scope_find_non_existent() -> Result<()> {
        let scope: Scope<u32> = Scope::new();

        assert!(scope.find("test").is_err());

        Ok(())
    }
}
