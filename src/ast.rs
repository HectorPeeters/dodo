use std::fmt;

use crate::error::Result;
use crate::tokenizer::{SourceRange, TokenType};
use crate::types::TypeId;

pub type Annotations<'a> = Vec<(&'a str, Option<Expression<'a>>)>;

#[derive(Debug, Clone, PartialEq)]
pub enum UpperStatement<'a> {
    Function(
        String,
        Vec<(&'a str, TypeId)>,
        TypeId,
        Statement<'a>,
        Annotations<'a>,
        SourceRange,
    ),
    StructDeclaratin(&'a str, Vec<(&'a str, TypeId)>),
    ConstDeclaration(
        &'a str,
        TypeId,
        Expression<'a>,
        Annotations<'a>,
        SourceRange,
    ),
    ExternDeclaration(&'a str, SourceRange),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<'a> {
    Block(Vec<Statement<'a>>, bool, SourceRange),
    Declaration(&'a str, TypeId, SourceRange),
    Assignment(Expression<'a>, Expression<'a>, SourceRange),
    Expression(Expression<'a>, SourceRange),
    While(Expression<'a>, Box<Statement<'a>>, SourceRange),
    If(
        Expression<'a>,
        Box<Statement<'a>>,
        Option<Box<Statement<'a>>>,
        SourceRange,
    ),
    Return(Expression<'a>, SourceRange),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinaryOperatorType {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    ShiftLeft,
    ShiftRight,
    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    LogicalOr,
    LogicalAnd,
}

impl BinaryOperatorType {
    pub fn from_token_type(token_type: TokenType) -> Self {
        match token_type {
            TokenType::Plus => BinaryOperatorType::Add,
            TokenType::Minus => BinaryOperatorType::Subtract,
            TokenType::Asterisk => BinaryOperatorType::Multiply,
            TokenType::Slash => BinaryOperatorType::Divide,
            TokenType::Percent => BinaryOperatorType::Modulo,
            TokenType::DoubleLessThan => BinaryOperatorType::ShiftLeft,
            TokenType::DoubleGreaterThan => BinaryOperatorType::ShiftRight,
            TokenType::DoubleEqual => BinaryOperatorType::Equal,
            TokenType::NotEqual => BinaryOperatorType::NotEqual,
            TokenType::LessThan => BinaryOperatorType::LessThan,
            TokenType::LessThanEqual => BinaryOperatorType::LessThanEqual,
            TokenType::GreaterThan => BinaryOperatorType::GreaterThan,
            TokenType::GreaterThanEqual => BinaryOperatorType::GreaterThanEqual,
            TokenType::DoubleVerticalBar => BinaryOperatorType::LogicalOr,
            TokenType::DoubleAmpersand => BinaryOperatorType::LogicalAnd,
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

#[derive(Debug, Clone, PartialEq, Eq)]
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
pub struct EscapedString<'a> {
    inner: &'a str,
}

impl<'a> EscapedString<'a> {
    pub fn new(inner: &'a str) -> Self {
        Self { inner }
    }

    pub fn inner(&self) -> &'a str {
        self.inner
    }
}

impl<'a> From<&'a str> for EscapedString<'a> {
    fn from(value: &'a str) -> Self {
        Self::new(value)
    }
}

impl<'a> fmt::Display for EscapedString<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.inner
                .replace("\\\"", "\"")
                .replace("\\t", "\t")
                .replace("\\n", "\n")
                .replace("\\r", "\r")
                .replace("\\'", "'")
                .replace("\\\"", "\"")
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression<'a> {
    BinaryOperator(
        BinaryOperatorType,
        Box<Expression<'a>>,
        Box<Expression<'a>>,
        TypeId,
        SourceRange,
    ),
    UnaryOperator(UnaryOperatorType, Box<Expression<'a>>, TypeId, SourceRange),
    FunctionCall(&'a str, Vec<Expression<'a>>, TypeId, SourceRange),
    IntegerLiteral(u64, TypeId, SourceRange),
    BooleanLiteral(bool, TypeId, SourceRange),
    VariableRef(&'a str, TypeId, SourceRange),
    StringLiteral(EscapedString<'a>, TypeId, SourceRange),
    StructLiteral(Vec<(&'a str, Expression<'a>)>, TypeId, SourceRange),
    FieldAccessor(&'a str, Box<Expression<'a>>, TypeId, SourceRange),
    Widen(Box<Expression<'a>>, TypeId, SourceRange),
    Cast(Box<Expression<'a>>, TypeId, SourceRange),
    Type(TypeId, SourceRange),
}

impl<'a> Expression<'a> {
    pub fn get_type(&self) -> TypeId {
        use Expression::*;

        match self {
            BinaryOperator(_, _, _, t, _) => *t,
            UnaryOperator(_, _, t, _) => *t,
            FunctionCall(_, _, t, _) => *t,
            IntegerLiteral(_, t, _) => *t,
            BooleanLiteral(_, t, _) => *t,
            VariableRef(_, t, _) => *t,
            StringLiteral(_, t, _) => *t,
            StructLiteral(_, t, _) => *t,
            FieldAccessor(_, _, t, _) => *t,
            Widen(_, t, _) => *t,
            Cast(_, t, _) => *t,
            Type(t, _) => *t,
        }
    }

    pub fn range(&self) -> &SourceRange {
        use Expression::*;

        match self {
            BinaryOperator(_, _, _, _, r) => r,
            UnaryOperator(_, _, _, r) => r,
            FunctionCall(_, _, _, r) => r,
            IntegerLiteral(_, _, r) => r,
            BooleanLiteral(_, _, r) => r,
            VariableRef(_, _, r) => r,
            StringLiteral(_, _, r) => r,
            StructLiteral(_, _, r) => r,
            FieldAccessor(_, _, _, r) => r,
            Widen(_, _, r) => r,
            Cast(_, _, r) => r,
            Type(_, r) => r,
        }
    }
}

pub trait AstTransformer<U, S, E> {
    fn transform_upper_statement(&mut self, statement: UpperStatement) -> Result<U>;

    fn transform_statement(&mut self, statement: Statement) -> Result<S>;

    fn transform_expression(&mut self, expression: Expression) -> Result<E>;
}

pub trait ConsumingAstVisitor<'a, U, S, E> {
    fn visit_upper_statement(&mut self, statement: UpperStatement<'a>) -> Result<U>;

    fn visit_statement(&mut self, statement: Statement<'a>) -> Result<S>;

    fn visit_expression(&mut self, expression: Expression<'a>) -> Result<E>;
}
