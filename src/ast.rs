use crate::error::Result;
use crate::tokenizer::{SourceRange, TokenType};
use crate::types::TypeId;

#[derive(Debug, Clone, PartialEq)]
pub enum UpperStatement {
    Function(
        String,
        Vec<(String, TypeId)>,
        TypeId,
        Box<Statement>,
        Vec<(String, Option<Expression>)>,
        SourceRange,
    ),
    StructDeclaratin(String, Vec<(String, TypeId)>),
    ConstDeclaration(String, TypeId, Expression, SourceRange),
    ExternDeclaration(String, SourceRange),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Block(Vec<Statement>, bool, SourceRange),
    Declaration(String, TypeId, SourceRange),
    Assignment(Expression, Expression, SourceRange),
    Expression(Expression, SourceRange),
    While(Expression, Box<Statement>, SourceRange),
    If(Expression, Box<Statement>, SourceRange),
    Return(Expression, SourceRange),
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
pub enum Expression {
    BinaryOperator(
        BinaryOperatorType,
        Box<Expression>,
        Box<Expression>,
        TypeId,
        SourceRange,
    ),
    UnaryOperator(UnaryOperatorType, Box<Expression>, TypeId, SourceRange),
    FunctionCall(String, Vec<Expression>, TypeId, SourceRange),
    IntegerLiteral(u64, TypeId, SourceRange),
    BooleanLiteral(bool, TypeId, SourceRange),
    VariableRef(String, TypeId, SourceRange),
    StringLiteral(String, TypeId, SourceRange),
    FieldAccessor(String, Box<Expression>, TypeId, SourceRange),
    Widen(Box<Expression>, TypeId, SourceRange),
}

impl Expression {
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
            FieldAccessor(_, _, t, _) => *t,
            Widen(_, t, _) => *t,
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
            FieldAccessor(_, _, _, r) => r,
            Widen(_, _, r) => r,
        }
    }
}

pub trait AstTransformer<U, S, E> {
    fn transform_upper_statement(&mut self, statement: UpperStatement) -> Result<U>;

    fn transform_statement(&mut self, statement: Statement) -> Result<S>;

    fn transform_expression(&mut self, expression: Expression) -> Result<E>;
}

pub trait ConsumingAstVisitor<U, S, E> {
    fn visit_upper_statement(&mut self, statement: UpperStatement) -> Result<U>;

    fn visit_statement(&mut self, statement: Statement) -> Result<S>;

    fn visit_expression(&mut self, expression: Expression) -> Result<E>;
}
