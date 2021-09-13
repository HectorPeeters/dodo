#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Nil(),
    UInt8(),
    UInt16(),
    UInt32(),
    Ref(Box<Type>),
}

impl Type {
    ///
    /// Gets the size in bits of a specific type
    ///
    pub fn size(&self) -> usize {
        use Type::*;

        match self {
            Nil() => unreachable!("We should never get the size of Nil"),
            UInt8() => 8,
            UInt16() => 16,
            UInt32() => 32,
            Ref(_) => 32,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn correct_size() {
        assert_eq!(Type::UInt8().size(), 8);
        assert_eq!(Type::UInt16().size(), 16);
        assert_eq!(Type::UInt32().size(), 32);
        assert_eq!(Type::Ref(Box::new(Type::Nil())).size(), 32);
    }
}
