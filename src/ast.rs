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
}

impl BinaryOperatorType {
    pub fn from_token_type(token_type: TokenType) -> Self {
        match token_type {
            TokenType::Plus => BinaryOperatorType::Add,
            TokenType::Minus => BinaryOperatorType::Subtract,
            TokenType::Asterix => BinaryOperatorType::Multiply,
            TokenType::Slash => BinaryOperatorType::Divide,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperatorType {
    Negate,
}

impl UnaryOperatorType {
    pub fn from_token_type(token_type: TokenType) -> Self {
        match token_type {
            TokenType::Minus => UnaryOperatorType::Negate,
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
}
