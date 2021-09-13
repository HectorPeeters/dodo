use dodo_core::Type;

// STATEMENTS

#[derive(Debug, Clone, PartialEq)]
pub struct BlockStatement {
    pub children: Vec<Statement>,
}

impl BlockStatement {
    pub fn new(children: Vec<Statement>) -> Self {
        Self { children }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct DeclarationStatement {
    pub name: String,
    pub variable_type: Type,
}

impl DeclarationStatement {
    pub fn new(name: String, variable_type: Type) -> Self {
        Self {
            name,
            variable_type,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignmentStatement {
    pub name: String,
    pub value: Expression,
}

impl AssignmentStatement {
    pub fn new(name: String, value: Expression) -> Self {
        Self { name, value }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionStatement {
    pub expression: Expression,
}

impl ExpressionStatement {
    pub fn new(expression: Expression) -> Self {
        Self { expression }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStatement {
    pub value: Expression,
}

impl ReturnStatement {
    pub fn new(value: Expression) -> Self {
        Self { value }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionStatement {
    pub name: String,
    pub arg_types: Vec<Type>,
    pub return_type: Type,
    pub body: Box<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Block(BlockStatement),
    Declaration(DeclarationStatement),
    Assignment(AssignmentStatement),
    Expression(ExpressionStatement),
    Return(ReturnStatement),
    Function(FunctionStatement),
}

pub type Value = u32;

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperatorType {
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperatorType {
    Negate,
    Ref,
    Deref,
}

// EXPRESSIONS

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryOperatorExpression {
    pub op_type: BinaryOperatorType,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

impl BinaryOperatorExpression {
    pub fn new(op_type: BinaryOperatorType, left: Box<Expression>, right: Box<Expression>) -> Self {
        Self {
            op_type,
            left,
            right,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryOperatorExpression {
    pub op_type: UnaryOperatorType,
    pub expr: Box<Expression>,
}

impl UnaryOperatorExpression {
    pub fn new(op_type: UnaryOperatorType, expr: Box<Expression>) -> Self {
        Self { op_type, expr }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCallExpression {
    pub name: String,
    pub args: Vec<Expression>,
}

impl FunctionCallExpression {
    pub fn new(name: String, args: Vec<Expression>) -> Self {
        Self { name, args }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CastExpression {
    pub expr: Box<Expression>,
    pub cast_type: Type,
}

impl CastExpression {
    pub fn new(expr: Box<Expression>, cast_type: Type) -> Self {
        Self { expr, cast_type }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstantExpression {
    pub value: Value,
    pub value_type: Type,
}

impl ConstantExpression {
    pub fn new(value: Value, value_type: Type) -> Self {
        Self { value, value_type }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    BinaryOperator(BinaryOperatorExpression),
    UnaryOperator(UnaryOperatorExpression),
    FunctionCall(FunctionCallExpression),
    Cast(CastExpression),
    Constant(ConstantExpression),
}
