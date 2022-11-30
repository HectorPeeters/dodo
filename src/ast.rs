use std::fmt;

use crate::error::Result;
use crate::tokenizer::{SourceRange, TokenType};
use crate::types::TypeId;

pub type Annotations<'a> = Vec<(&'a str, Option<Expression<'a>>)>;

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
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

#[derive(Debug, Copy, Clone, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub struct BinaryOperatorExpr<'a> {
    pub op_type: BinaryOperatorType,
    pub left: Box<Expression<'a>>,
    pub right: Box<Expression<'a>>,
    pub type_id: TypeId,
    pub range: SourceRange,
}

#[derive(Debug, PartialEq)]
pub struct UnaryOperatorExpr<'a> {
    pub op_type: UnaryOperatorType,
    pub expr: Box<Expression<'a>>,
    pub type_id: TypeId,
    pub range: SourceRange,
}

#[derive(Debug, PartialEq)]
pub struct FunctionCallExpr<'a> {
    pub name: &'a str,
    pub args: Vec<Expression<'a>>,
    pub type_id: TypeId,
    pub range: SourceRange,
}

#[derive(Debug, PartialEq)]
pub struct IntegerLiteralExpr {
    pub value: u64,
    pub type_id: TypeId,
    pub range: SourceRange,
}

#[derive(Debug, PartialEq)]
pub struct BooleanLiteralExpr {
    pub value: bool,
    pub type_id: TypeId,
    pub range: SourceRange,
}

#[derive(Debug, PartialEq)]
pub struct VariableRefExpr<'a> {
    pub name: &'a str,
    pub type_id: TypeId,
    pub range: SourceRange,
}

#[derive(Debug, PartialEq)]
pub struct StringLiteralExpr<'a> {
    pub value: EscapedString<'a>,
    pub type_id: TypeId,
    pub range: SourceRange,
}

#[derive(Debug, PartialEq)]
pub struct StructLiteralExpr<'a> {
    pub fields: Vec<(&'a str, Expression<'a>)>,
    pub type_id: TypeId,
    pub range: SourceRange,
}

#[derive(Debug, PartialEq)]
pub struct FieldAccessorExpr<'a> {
    pub name: &'a str,
    pub expr: Box<Expression<'a>>,
    pub type_id: TypeId,
    pub range: SourceRange,
}

#[derive(Debug, PartialEq)]
pub struct WidenExpr<'a> {
    pub expr: Box<Expression<'a>>,
    pub type_id: TypeId,
    pub range: SourceRange,
}

#[derive(Debug, PartialEq)]
pub struct CastExpr<'a> {
    pub expr: Box<Expression<'a>>,
    pub type_id: TypeId,
    pub range: SourceRange,
}

#[derive(Debug, PartialEq)]
pub struct TypeExpr {
    pub type_id: TypeId,
    pub range: SourceRange,
}

#[derive(Debug, PartialEq)]
pub enum Expression<'a> {
    BinaryOperator(BinaryOperatorExpr<'a>),
    UnaryOperator(UnaryOperatorExpr<'a>),
    FunctionCall(FunctionCallExpr<'a>),
    IntegerLiteral(IntegerLiteralExpr),
    BooleanLiteral(BooleanLiteralExpr),
    VariableRef(VariableRefExpr<'a>),
    StringLiteral(StringLiteralExpr<'a>),
    StructLiteral(StructLiteralExpr<'a>),
    FieldAccessor(FieldAccessorExpr<'a>),
    Widen(WidenExpr<'a>),
    Cast(CastExpr<'a>),
    Type(TypeExpr),
}

impl<'a> Expression<'a> {
    pub fn get_type(&self) -> TypeId {
        use Expression::*;

        match self {
            BinaryOperator(x) => x.type_id,
            UnaryOperator(x) => x.type_id,
            FunctionCall(x) => x.type_id,
            IntegerLiteral(x) => x.type_id,
            BooleanLiteral(x) => x.type_id,
            VariableRef(x) => x.type_id,
            StringLiteral(x) => x.type_id,
            StructLiteral(x) => x.type_id,
            FieldAccessor(x) => x.type_id,
            Widen(x) => x.type_id,
            Cast(x) => x.type_id,
            Type(x) => x.type_id,
        }
    }

    pub fn range(&self) -> &SourceRange {
        use Expression::*;

        match self {
            BinaryOperator(x) => &x.range,
            UnaryOperator(x) => &x.range,
            FunctionCall(x) => &x.range,
            IntegerLiteral(x) => &x.range,
            BooleanLiteral(x) => &x.range,
            VariableRef(x) => &x.range,
            StringLiteral(x) => &x.range,
            StructLiteral(x) => &x.range,
            FieldAccessor(x) => &x.range,
            Widen(x) => &x.range,
            Cast(x) => &x.range,
            Type(x) => &x.range,
        }
    }
}

pub trait AstTransformer<'a, U, S, E> {
    fn visit_upper_statement(&mut self, statement: UpperStatement<'a>) -> Result<U>;

    fn visit_statement(&mut self, statement: Statement<'a>) -> Result<S>;

    fn visit_expression(&mut self, expression: Expression<'a>) -> Result<E>;
}
