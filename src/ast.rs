use crate::error::Result;
use crate::types::Type;

use crate::tokenizer::{SourceRange, TokenType};

pub type TypedStatement = Statement<Type>;
pub type TypedExpression = Expression<Type>;

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<T> {
    Block(Vec<Statement<T>>, bool, T, SourceRange),
    Declaration(String, Type, T, SourceRange),
    Assignment(String, Expression<T>, T, SourceRange),
    Expression(Expression<T>, T, SourceRange),
    While(Expression<T>, Box<Statement<T>>, T, SourceRange),
    If(Expression<T>, Box<Statement<T>>, T, SourceRange),
    Return(Expression<T>, T, SourceRange),
    Function(
        String,
        Vec<(String, Type)>,
        Type,
        Box<Statement<T>>,
        T,
        SourceRange,
    ),
}

impl<T> Statement<T> {
    pub fn data(&self) -> &T {
        use Statement::*;

        match self {
            Block(_, _, t, _) => t,
            Declaration(_, _, t, _) => t,
            Assignment(_, _, t, _) => t,
            Expression(_, t, _) => t,
            While(_, _, t, _) => t,
            If(_, _, t, _) => t,
            Return(_, t, _) => t,
            Function(_, _, _, _, t, _) => t,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
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
pub enum Expression<T> {
    BinaryOperator(
        BinaryOperatorType,
        Box<Expression<T>>,
        Box<Expression<T>>,
        T,
        SourceRange,
    ),
    UnaryOperator(UnaryOperatorType, Box<Expression<T>>, T, SourceRange),
    FunctionCall(String, Vec<Expression<T>>, T, SourceRange),
    Literal(u64, T, SourceRange),
    VariableRef(String, T, SourceRange),
    StringLiteral(String, T, SourceRange),
    Widen(Box<Expression<T>>, T, SourceRange),
}

impl<T> Expression<T> {
    pub fn data(&self) -> &T {
        use Expression::*;

        match self {
            BinaryOperator(_, _, _, t, _) => t,
            UnaryOperator(_, _, t, _) => t,
            FunctionCall(_, _, t, _) => t,
            Literal(_, t, _) => t,
            VariableRef(_, t, _) => t,
            StringLiteral(_, t, _) => t,
            Widen(_, t, _) => t,
        }
    }
}

pub trait AstTransformer<S, T> {
    fn transform_statement(&mut self, statement: Statement<S>) -> Result<Statement<T>>;

    fn transform_expression(&mut self, expression: Expression<S>) -> Result<Expression<T>>;
}
