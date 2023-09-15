use std::collections::HashMap;
use std::fmt;

use crate::error::Result;
use crate::id_impl;
use crate::lexer::{SourceRange, TokenType};
use crate::sema::{
    DeclarationId, BUILTIN_TYPE_U16, BUILTIN_TYPE_U32, BUILTIN_TYPE_U64, BUILTIN_TYPE_U8,
};
use crate::types::TypeId;

pub struct Ast<'a> {
    pub expressions: Vec<Expression<'a>>,
    pub statements: Vec<Statement>,
    pub upper_statements: Vec<UpperStatement<'a>>,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct ExpressionId(u32);
id_impl!(ExpressionId);

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct StatementId(u32);
id_impl!(StatementId);

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct UpperStatementId(u32);
id_impl!(UpperStatementId);

impl<'a> Default for Ast<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> Ast<'a> {
    pub fn new() -> Self {
        Self {
            expressions: vec![],
            statements: vec![],
            upper_statements: vec![],
        }
    }

    pub fn add_expression(&mut self, expression: Expression<'a>) -> ExpressionId {
        self.expressions.push(expression);
        ExpressionId(self.expressions.len() as u32 - 1)
    }

    pub fn add_statement(&mut self, statements: Statement) -> StatementId {
        self.statements.push(statements);
        StatementId(self.statements.len() as u32 - 1)
    }

    pub fn add_upper_statement(
        &mut self,
        upper_statements: UpperStatement<'a>,
    ) -> UpperStatementId {
        self.upper_statements.push(upper_statements);
        UpperStatementId(self.upper_statements.len() as u32 - 1)
    }

    pub fn get_expression(&self, expression_id: ExpressionId) -> &Expression<'a> {
        &self.expressions[*expression_id as usize]
    }

    pub fn get_statement(&self, statement_id: StatementId) -> &Statement {
        &self.statements[*statement_id as usize]
    }

    pub fn get_upper_statement(&self, upper_statement_id: UpperStatementId) -> &UpperStatement<'a> {
        &self.upper_statements[*upper_statement_id as usize]
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Annotations<'a>(HashMap<&'a str, Option<ExpressionId>>);

impl<'a> Annotations<'a> {
    pub fn empty() -> Self {
        Self(HashMap::new())
    }

    pub fn get(&self, name: &str) -> Option<&Option<ExpressionId>> {
        self.0.get(name)
    }

    pub fn get_string(&self, name: &str, ast: &Ast<'a>) -> Option<&'a str> {
        match self.0.get(name) {
            Some(Some(expr_id)) => match ast.get_expression(*expr_id) {
                // TODO: this doens't handle escaping
                Expression::StringLiteral(value) => Some(value.value.inner()),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn has_flag(&self, name: &str) -> bool {
        self.0.get(name).is_some()
    }
}

impl<'a> From<HashMap<&'a str, Option<ExpressionId>>> for Annotations<'a> {
    fn from(value: HashMap<&'a str, Option<ExpressionId>>) -> Self {
        Self(value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDeclaration<'a> {
    pub name: &'a str,
    pub params: Vec<DeclarationId>,
    pub return_type: TypeId,
    pub body: StatementId,
    pub annotations: Annotations<'a>,
    pub range: SourceRange,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDeclaration<'a> {
    pub name: &'a str,
    pub declaration_id: DeclarationId,
    pub fields: Vec<(&'a str, TypeId)>,
    pub range: SourceRange,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstDeclaration<'a> {
    pub declaration_id: DeclarationId,
    pub value: ExpressionId,
    pub annotations: Annotations<'a>,
    pub range: SourceRange,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExternDeclaration<'a> {
    pub name: &'a str,
    pub range: SourceRange,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UpperStatement<'a> {
    Function(FunctionDeclaration<'a>),
    StructDeclaration(StructDeclaration<'a>),
    ConstDeclaration(ConstDeclaration<'a>),
    ExternDeclaration(ExternDeclaration<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockStatement {
    pub children: Vec<StatementId>,
    pub scoped: bool,
    pub range: SourceRange,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DeclarationStatement {
    pub declaration_id: DeclarationId,
    pub range: SourceRange,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignmentStatement {
    pub left: ExpressionId,
    pub right: ExpressionId,
    pub range: SourceRange,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionStatement {
    pub expr: ExpressionId,
    pub range: SourceRange,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileStatement {
    pub condition: ExpressionId,
    pub body: StatementId,
    pub range: SourceRange,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStatement {
    pub condition: ExpressionId,
    pub if_body: StatementId,
    pub else_body: Option<StatementId>,
    pub range: SourceRange,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStatement {
    pub expr: ExpressionId,
    pub range: SourceRange,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Block(BlockStatement),
    Declaration(DeclarationStatement),
    Assignment(AssignmentStatement),
    Expression(ExpressionStatement),
    While(WhileStatement),
    If(IfStatement),
    Return(ReturnStatement),
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

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryOperatorExpr {
    pub op_type: BinaryOperatorType,
    pub left: ExpressionId,
    pub right: ExpressionId,
    pub type_id: TypeId,
    pub range: SourceRange,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryOperatorExpr {
    pub op_type: UnaryOperatorType,
    pub expr: ExpressionId,
    pub type_id: TypeId,
    pub range: SourceRange,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCallExpr<'a> {
    pub name: &'a str,
    pub args: Vec<ExpressionId>,
    pub type_id: TypeId,
    pub range: SourceRange,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IntegerLiteralExpr {
    pub value: u64,
    pub type_id: TypeId,
    pub range: SourceRange,
}

impl IntegerLiteralExpr {
    pub fn new(value: u64, range: SourceRange) -> Self {
        let type_id = if value <= 255 {
            BUILTIN_TYPE_U8
        } else if value <= 65535 {
            BUILTIN_TYPE_U16
        } else if value <= 4294967295 {
            BUILTIN_TYPE_U32
        } else {
            BUILTIN_TYPE_U64
        };

        Self {
            value,
            type_id,
            range,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BooleanLiteralExpr {
    pub value: bool,
    pub type_id: TypeId,
    pub range: SourceRange,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableRefExpr<'a> {
    pub name: &'a str,
    pub declaration_id: DeclarationId,
    pub type_id: TypeId,
    pub range: SourceRange,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StringLiteralExpr<'a> {
    pub value: EscapedString<'a>,
    pub type_id: TypeId,
    pub range: SourceRange,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructLiteralExpr<'a> {
    pub fields: Vec<(&'a str, ExpressionId)>,
    pub type_id: TypeId,
    pub range: SourceRange,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldAccessorExpr<'a> {
    pub name: &'a str,
    pub expr: ExpressionId,
    pub type_id: TypeId,
    pub range: SourceRange,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WidenExpr {
    pub expr: ExpressionId,
    pub type_id: TypeId,
    pub range: SourceRange,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CastExpr {
    pub expr: ExpressionId,
    pub type_id: TypeId,
    pub range: SourceRange,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeExpr {
    pub type_id: TypeId,
    pub range: SourceRange,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression<'a> {
    BinaryOperator(BinaryOperatorExpr),
    UnaryOperator(UnaryOperatorExpr),
    FunctionCall(FunctionCallExpr<'a>),
    IntegerLiteral(IntegerLiteralExpr),
    BooleanLiteral(BooleanLiteralExpr),
    VariableRef(VariableRefExpr<'a>),
    StringLiteral(StringLiteralExpr<'a>),
    StructLiteral(StructLiteralExpr<'a>),
    FieldAccessor(FieldAccessorExpr<'a>),
    Widen(WidenExpr),
    Cast(CastExpr),
    Type(TypeExpr),
}

impl<'a> Expression<'a> {
    pub fn type_id(&self) -> TypeId {
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

pub trait AstVisitor<'a, U, S, E> {
    fn visit_upper_statement(&mut self, statement: UpperStatementId) -> Result<U>;

    fn visit_statement(&mut self, statement: StatementId) -> Result<S>;

    fn visit_expression(&mut self, expression: ExpressionId) -> Result<E>;
}
