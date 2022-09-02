use crate::{
    ast::{BinaryOperatorType, UnaryOperatorType},
    tokenizer::SourceRange,
    types::TypeId,
};

#[derive(Debug, Clone, PartialEq)]
pub enum TypedUpperStatement {
    Function(
        String,
        Vec<(String, TypeId)>,
        TypeId,
        Box<TypedStatement>,
        Vec<(String, Option<TypedExpression>)>,
        SourceRange,
    ),
    ConstDeclaration(String, TypeId, TypedExpression, SourceRange),
    ExternDeclaration(String, SourceRange),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypedStatement {
    Block(Vec<TypedStatement>, bool, SourceRange),
    Declaration(String, TypeId, SourceRange),
    Assignment(TypedExpression, TypedExpression, SourceRange),
    Expression(TypedExpression, SourceRange),
    While(TypedExpression, Box<TypedStatement>, SourceRange),
    If(TypedExpression, Box<TypedStatement>, SourceRange),
    Return(TypedExpression, SourceRange),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypedExpression {
    BinaryOperator(
        BinaryOperatorType,
        Box<TypedExpression>,
        Box<TypedExpression>,
        TypeId,
        SourceRange,
    ),
    UnaryOperator(UnaryOperatorType, Box<TypedExpression>, TypeId, SourceRange),
    FunctionCall(String, Vec<TypedExpression>, TypeId, SourceRange),
    IntegerLiteral(u64, TypeId, SourceRange),
    BooleanLiteral(bool, TypeId, SourceRange),
    VariableRef(String, TypeId, SourceRange),
    StringLiteral(String, TypeId, SourceRange),
    Widen(Box<TypedExpression>, TypeId, SourceRange),
}

impl TypedExpression {
    pub fn get_type(&self) -> TypeId {
        match self {
            TypedExpression::BinaryOperator(_, _, _, t, _) => *t,
            TypedExpression::UnaryOperator(_, _, t, _) => *t,
            TypedExpression::FunctionCall(_, _, t, _) => *t,
            TypedExpression::IntegerLiteral(_, t, _) => *t,
            TypedExpression::BooleanLiteral(_, t, _) => *t,
            TypedExpression::VariableRef(_, t, _) => *t,
            TypedExpression::StringLiteral(_, t, _) => *t,
            TypedExpression::Widen(_, t, _) => *t,
        }
    }
}
