use crate::types::Type;

use crate::tokenizer::{SourceRange, TokenType};

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Block(Vec<Statement>, bool, SourceRange),
    Declaration(String, Type, SourceRange),
    Assignment(String, Expression, SourceRange),
    Expression(Expression, SourceRange),
    While(Expression, Box<Statement>, SourceRange),
    If(Expression, Box<Statement>, SourceRange),
    Return(Expression, SourceRange),
    Function(
        String,
        Vec<(String, Type)>,
        Type,
        Box<Statement>,
        SourceRange,
    ),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperatorType {
    Add,
    Subtract,
    Multiply,
    Divide,
    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
}

impl BinaryOperatorType {
    pub fn from_token_type(token_type: TokenType) -> Self {
        match token_type {
            TokenType::Plus => BinaryOperatorType::Add,
            TokenType::Minus => BinaryOperatorType::Subtract,
            TokenType::Asterisk => BinaryOperatorType::Multiply,
            TokenType::Slash => BinaryOperatorType::Divide,
            TokenType::DoubleEqual => BinaryOperatorType::Equal,
            TokenType::NotEqual => BinaryOperatorType::NotEqual,
            TokenType::LessThan => BinaryOperatorType::LessThan,
            TokenType::LessThanEqual => BinaryOperatorType::LessThanEqual,
            TokenType::GreaterThan => BinaryOperatorType::GreaterThan,
            TokenType::GreaterThanEqual => BinaryOperatorType::GreaterThanEqual,
            _ => unreachable!(),
        }
    }

    pub fn is_comparison(&self) -> bool {
        use BinaryOperatorType::*;
        matches!(
            self,
            Equal | NotEqual | LessThan | LessThanEqual | GreaterThan | GreaterThanEqual
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperatorType {
    Negate,
    Ref,
    Deref,
}

impl UnaryOperatorType {
    pub fn from_token_type(token_type: TokenType) -> Self {
        match token_type {
            TokenType::Minus => UnaryOperatorType::Negate,
            TokenType::Ampersand => UnaryOperatorType::Ref,
            TokenType::Asterisk => UnaryOperatorType::Deref,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    BinaryOperator(
        BinaryOperatorType,
        Box<Expression>,
        Box<Expression>,
        SourceRange,
    ),
    UnaryOperator(UnaryOperatorType, Box<Expression>, SourceRange),
    FunctionCall(String, Vec<Expression>, SourceRange),
    Literal(u64, Type, SourceRange),
    VariableRef(String, SourceRange),
    StringLiteral(String, SourceRange),
    Widen(Box<Expression>, Type, SourceRange),
}
