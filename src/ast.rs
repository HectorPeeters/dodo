use crate::error::Result;
use crate::tokenizer::{SourceRange, TokenType};
use crate::types::Type;

pub type TypedUpperStatement = UpperStatement<Type>;
pub type TypedStatement = Statement<Type>;
pub type TypedExpression = Expression<Type>;

#[derive(Debug, Clone, PartialEq)]
pub enum UpperStatement<T> {
    Function(
        String,
        Vec<(String, Type)>,
        Type,
        Box<Statement<T>>,
        Vec<(String, Option<Expression<T>>)>,
        SourceRange,
    ),
    ConstDeclaration(String, Type, Expression<T>, SourceRange),
}

impl<T> UpperStatement<T> {
    pub fn get_type(&self) -> &Type {
        match self {
            UpperStatement::Function(_, _, t, _, _, _) => t,
            UpperStatement::ConstDeclaration(_, t, _, _) => t,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<T> {
    Block(Vec<Statement<T>>, bool, T, SourceRange),
    Declaration(String, Type, T, SourceRange),
    Assignment(String, Expression<T>, T, SourceRange),
    Expression(Expression<T>, T, SourceRange),
    While(Expression<T>, Box<Statement<T>>, T, SourceRange),
    If(Expression<T>, Box<Statement<T>>, T, SourceRange),
    Return(Expression<T>, T, SourceRange),
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
        }
    }
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
    fn transform_upper_statement(
        &mut self,
        statement: UpperStatement<S>,
    ) -> Result<UpperStatement<T>>;

    fn transform_statement(&mut self, statement: Statement<S>) -> Result<Statement<T>>;

    fn transform_expression(&mut self, expression: Expression<S>) -> Result<Expression<T>>;
}

pub trait ConsumingAstVisitor<T, RS, RE> {
    fn visit_upper_statement(&mut self, statement: UpperStatement<T>) -> Result<RS>;

    fn visit_statement(&mut self, statement: Statement<T>) -> Result<RS>;

    fn visit_expression(&mut self, expression: Expression<T>) -> Result<RE>;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn statement_data() -> Result<()> {
        let expression = Expression::Literal(13, 14, (0..0).into());

        let ast = Statement::Block(vec![], false, 12, (0..0).into());
        assert_eq!(*ast.data(), 12);

        let ast = Statement::Declaration("test".to_string(), Type::Void(), 12, (0..0).into());
        assert_eq!(*ast.data(), 12);

        let ast = Statement::Assignment("test".to_string(), expression.clone(), 12, (0..0).into());
        assert_eq!(*ast.data(), 12);

        let ast = Statement::Expression(expression.clone(), 12, (0..0).into());
        assert_eq!(*ast.data(), 12);

        let ast = Statement::While(expression.clone(), Box::new(ast), 12, (0..0).into());
        assert_eq!(*ast.data(), 12);

        let ast = Statement::If(expression.clone(), Box::new(ast), 12, (0..0).into());
        assert_eq!(*ast.data(), 12);

        let ast = Statement::Return(expression, 12, (0..0).into());
        assert_eq!(*ast.data(), 12);

        Ok(())
    }
}
