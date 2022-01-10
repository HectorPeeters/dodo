#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    UInt8(),
    UInt16(),
    UInt32(),
    UInt64(),
    Ref(Box<Type>),
    Void(),
}

impl Type {
    ///
    /// Gets the size in bits of a specific type
    ///
    pub fn size(&self) -> usize {
        use Type::*;

        match self {
            UInt8() => 8,
            UInt16() => 16,
            UInt32() => 32,
            UInt64() => 64,
            Ref(_) => 64,
            Void() => unreachable!(),
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
        assert_eq!(Type::UInt64().size(), 64);
        assert_eq!(Type::Ref(Box::new(Type::UInt8())).size(), 64);
    }
}
