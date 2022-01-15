use crate::error::*;
use std::collections::HashMap;

pub struct Scope<T> {
    items: Vec<HashMap<String, T>>,
}

impl<T: Copy> Scope<T> {
    pub fn new() -> Self {
        Self { items: vec![] }
    }

    pub fn insert(&mut self, name: &str, data: T) -> Result<()> {
        match self.items.last_mut() {
            Some(last_scope) => {
                if last_scope.contains_key(name) {
                    Err(Error::Scope(
                        "Identifier already defined in scope".to_string(),
                    ))
                } else {
                    last_scope.insert(name.to_owned(), data);
                    Ok(())
                }
            }
            None => Err(Error::Scope(
                "Trying to insert into empty scope".to_string(),
            )),
        }
    }

    pub fn push(&mut self) {
        self.items.push(HashMap::new());
    }

    pub fn pop(&mut self) -> Result<()> {
        self.items
            .pop()
            .ok_or_else(|| Error::Scope("Tried popping while scope stack was empty".to_string()))
            .map(|_| ())
    }

    pub fn find(&self, name: &str) -> Result<T> {
        self.items
            .iter()
            .rev()
            .find_map(|x| x.get(name).copied())
            .ok_or_else(|| Error::Scope(format!("{} not found in scope", name)))
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
