use crate::ast::*;

pub trait StatementVisitor<T> {
    fn visit_block(&mut self, _block: &BlockStatement) -> T;
    fn visit_declaration(&mut self, _declaration: &DeclarationStatement) -> T;
    fn visit_assignment(&mut self, _assignment: &AssignmentStatement) -> T;
    fn visit_return(&mut self, _ret: &ReturnStatement) -> T;
    fn visit_function(&mut self, _function: &FunctionStatement) -> T;

    fn visit_statement(&mut self, stmt: &Statement) -> T {
        use Statement::*;
        match stmt {
            Block(block) => self.visit_block(block),
            Declaration(declaration) => self.visit_declaration(declaration),
            Assignment(assignment) => self.visit_assignment(assignment),
            Return(ret) => self.visit_return(ret),
            Function(function) => self.visit_function(function),
            Expression(expression) => todo!(),
        }
    }
}

pub trait ExpressionVisitor<T> {
    fn visit_binary_operator(&mut self, _binary: &BinaryOperatorExpression) -> T;
    fn visit_unary_operator(&mut self, _unary: &UnaryOperatorExpression) -> T;
    fn visit_function_call(&mut self, _function: &FunctionCallExpression) -> T;
    fn visit_cast(&mut self, _cast: &CastExpression) -> T;
    fn visit_constant(&mut self, _constant: &ConstantExpression) -> T;

    fn visit_expression(&mut self, expr: &Expression) -> T {
        use Expression::*;
        match expr {
            BinaryOperator(binary_operator) => self.visit_binary_operator(binary_operator),
            UnaryOperator(unary_operator) => self.visit_unary_operator(unary_operator),
            FunctionCall(function_call) => self.visit_function_call(function_call),
            Cast(cast) => self.visit_cast(cast),
            Constant(constant) => self.visit_constant(constant),
        }
    }
}

#[cfg(test)]
mod tests {
    use dodo_core::Type;

    use super::*;

    struct TestExpressionVisitor {}

    impl ExpressionVisitor<u32> for TestExpressionVisitor {
        fn visit_binary_operator(&mut self, binop: &BinaryOperatorExpression) -> u32 {
            self.visit_expression(&binop.left) + self.visit_expression(&binop.right) + 1
        }

        fn visit_unary_operator(&mut self, unop: &UnaryOperatorExpression) -> u32 {
            self.visit_expression(&unop.expr) + 1
        }

        fn visit_function_call(&mut self, function: &FunctionCallExpression) -> u32 {
            let mut result = 1;
            for arg in &function.args {
                result += self.visit_expression(arg)
            }
            result
        }

        fn visit_cast(&mut self, cast: &CastExpression) -> u32 {
            self.visit_expression(&cast.expr) + 1
        }

        fn visit_constant(&mut self, _constant: &ConstantExpression) -> u32 {
            1
        }
    }

    #[test]
    fn visitor_expression() {
        let ast = Expression::BinaryOperator(BinaryOperatorExpression::new(
            BinaryOperatorType::Add,
            Box::new(Expression::Constant(ConstantExpression::new(
                12,
                Type::UInt8(),
            ))),
            Box::new(Expression::FunctionCall(FunctionCallExpression::new(
                "".to_string(),
                vec![],
            ))),
        ));
        let mut visitor = TestExpressionVisitor {};
        assert_eq!(visitor.visit_expression(&ast), 3);
    }
}
