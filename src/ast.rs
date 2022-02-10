use crate::types::Type;

use crate::tokenizer::TokenType;

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<C> {
    Block(Vec<Statement<C>>, bool),
    Declaration(String, Type),
    Assignment(String, Expression<C>),
    Expression(Expression<C>),
    While(Expression<C>, Box<Statement<C>>),
    If(Expression<C>, Box<Statement<C>>),
    Return(Expression<C>),
    Function(String, Vec<(String, Type)>, Type, Box<Statement<C>>),
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
            TokenType::Asterix => BinaryOperatorType::Multiply,
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
            TokenType::Asterix => UnaryOperatorType::Deref,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression<C> {
    BinaryOperator(BinaryOperatorType, Box<Expression<C>>, Box<Expression<C>>),
    UnaryOperator(UnaryOperatorType, Box<Expression<C>>),
    FunctionCall(String, Vec<Expression<C>>),
    Literal(C, Type),
    VariableRef(String),
    StringLiteral(String),
    Widen(Box<Expression<C>>, Type),
}
