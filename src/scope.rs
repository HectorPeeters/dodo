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
                    Err(Error::ScopeError(
                        "Identifier already defined in scope".to_string(),
                    ))
                } else {
                    last_scope.insert(name.to_owned(), data);
                    Ok(())
                }
            }
            None => Err(Error::ScopeError(
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
            .ok_or(Error::ScopeError(
                "Tried popping while scope stack was empty".to_string(),
            ))
            .map(|_| ())
    }

    pub fn find(&self, name: &str) -> Result<T> {
        self.items
            .iter()
            .rev()
            .find_map(|x| x.get(name).map(|x| *x))
            .ok_or(Error::ScopeError(format!("{} not found in scope", name)))
    }

    pub fn len(&self) -> Result<usize> {
        self.items
            .last()
            .ok_or(Error::ScopeError(
                "Trying to get length of empty scope".to_string(),
            ))
            .map(|x| x.len())
    }
}
