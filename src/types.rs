#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    UInt8(),
    UInt16(),
    UInt32(),
    UInt64(),
    Bool(),
    Ref(Box<Type>),
    Void(),
}

impl Type {
    pub fn size(&self) -> usize {
        use Type::*;
        match self {
            UInt8() => 8,
            UInt16() => 16,
            UInt32() => 32,
            UInt64() => 64,
            Bool() => 8,
            Ref(_) => 64,
            Void() => unreachable!(),
        }
    }

    pub fn get_ref(self) -> Self {
        Type::Ref(Box::new(self))
    }
}
