use dodo_parser::ast::{Expression, Statement};

pub trait StatementVisitor<T, C> {
    fn visit_statement(&mut self, stmt: &Statement<C>) -> T;
}

pub trait ExpressionVisitor<T, C> {
    fn visit_expression(&mut self, expr: &Expression<C>) -> T;
}
