use crate::{
    ast::{
        AssignmentStatement, BinaryOperatorExpr, BlockStatement, BooleanLiteralExpr, CastExpr,
        ConstDeclaration, DeclarationStatement, Expression, ExpressionStatement, ExternDeclaration,
        FieldAccessorExpr, FunctionCallExpr, FunctionDeclaration, IfStatement, IntegerLiteralExpr,
        ReturnStatement, Statement, StringLiteralExpr, StructDeclaration, StructLiteralExpr,
        UnaryOperatorExpr, UpperStatement, VariableRefExpr, WhileStatement, WidenExpr,
    },
    error::Result,
};

pub trait AstWalker<'a> {
    fn visit_upper_statement(&mut self, stmt: UpperStatement<'a>) -> Result<UpperStatement<'a>> {
        match stmt {
            UpperStatement::Function(func_decl) => {
                let body = self.visit_statement(func_decl.body)?;

                self.visit_function_declaration(FunctionDeclaration {
                    name: func_decl.name,
                    params: func_decl.params,
                    return_type: func_decl.return_type,
                    body,
                    // TODO: annotations
                    annotations: func_decl.annotations,
                    range: func_decl.range,
                })
            }
            UpperStatement::StructDeclaration(struct_decl) => {
                self.visit_struct_declaration(struct_decl)
            }
            UpperStatement::ConstDeclaration(const_decl) => {
                let value = self.visit_expression(const_decl.value)?;

                self.visit_const_declaration(ConstDeclaration {
                    value,
                    // TODO: annotations
                    annotations: const_decl.annotations,
                    declaration_id: const_decl.declaration_id,
                    range: const_decl.range,
                })
            }
            UpperStatement::ExternDeclaration(extern_decl) => {
                self.visit_extern_declaration(extern_decl)
            }
        }
    }

    fn visit_function_declaration(
        &mut self,
        func_decl: FunctionDeclaration<'a>,
    ) -> Result<UpperStatement<'a>> {
        Ok(UpperStatement::Function(func_decl))
    }

    fn visit_struct_declaration(
        &mut self,
        struct_decl: StructDeclaration<'a>,
    ) -> Result<UpperStatement<'a>> {
        Ok(UpperStatement::StructDeclaration(struct_decl))
    }

    fn visit_const_declaration(
        &mut self,
        const_decl: ConstDeclaration<'a>,
    ) -> Result<UpperStatement<'a>> {
        Ok(UpperStatement::ConstDeclaration(const_decl))
    }

    fn visit_extern_declaration(
        &mut self,
        extern_decl: ExternDeclaration<'a>,
    ) -> Result<UpperStatement<'a>> {
        Ok(UpperStatement::ExternDeclaration(extern_decl))
    }

    fn visit_statement(&mut self, stmt: Statement<'a>) -> Result<Statement<'a>> {
        match stmt {
            Statement::Block(block_stmt) => {
                let children = block_stmt
                    .children
                    .into_iter()
                    .map(|x| self.visit_statement(x))
                    .collect::<Result<Vec<_>>>()?;

                self.visit_block_statement(BlockStatement {
                    children,
                    scoped: block_stmt.scoped,
                    range: block_stmt.range,
                })
            }
            Statement::Declaration(decl_stmt) => self.visit_declaration_statement(decl_stmt),
            Statement::Assignment(assign_stmt) => {
                let left = self.visit_expression(assign_stmt.left)?;
                let right = self.visit_expression(assign_stmt.right)?;

                self.visit_assignment_statement(AssignmentStatement {
                    left,
                    right,
                    range: assign_stmt.range,
                })
            }
            Statement::Expression(expr_stmt) => {
                let expr = self.visit_expression(expr_stmt.expr)?;

                self.visit_expression_statement(ExpressionStatement {
                    expr,
                    range: expr_stmt.range,
                })
            }
            Statement::While(while_stmt) => {
                let condition = self.visit_expression(while_stmt.condition)?;
                let body = Box::new(self.visit_statement(*while_stmt.body)?);

                self.visit_while_statement(WhileStatement {
                    condition,
                    body,
                    range: while_stmt.range,
                })
            }
            Statement::If(if_stmt) => {
                let condition = self.visit_expression(if_stmt.condition)?;
                let if_body = Box::new(self.visit_statement(*if_stmt.if_body)?);
                let else_body = if_stmt
                    .else_body
                    .map(|x| self.visit_statement(*x))
                    .map_or(Ok(None), |v| v.map(Some))?
                    .map(Box::new);

                self.visit_if_statement(IfStatement {
                    condition,
                    if_body,
                    else_body,
                    range: if_stmt.range,
                })
            }
            Statement::Return(ret_expr) => {
                let expr = self.visit_expression(ret_expr.expr)?;

                self.visit_return_statement(ReturnStatement {
                    expr,
                    range: ret_expr.range,
                })
            }
        }
    }

    fn visit_block_statement(&mut self, block_stmt: BlockStatement<'a>) -> Result<Statement<'a>> {
        Ok(Statement::Block(block_stmt))
    }

    fn visit_declaration_statement(
        &mut self,
        decl_stmt: DeclarationStatement,
    ) -> Result<Statement<'a>> {
        Ok(Statement::Declaration(decl_stmt))
    }

    fn visit_assignment_statement(
        &mut self,
        assign_stmt: AssignmentStatement<'a>,
    ) -> Result<Statement<'a>> {
        Ok(Statement::Assignment(assign_stmt))
    }

    fn visit_expression_statement(
        &mut self,
        expr_stmt: ExpressionStatement<'a>,
    ) -> Result<Statement<'a>> {
        Ok(Statement::Expression(expr_stmt))
    }

    fn visit_while_statement(&mut self, while_stmt: WhileStatement<'a>) -> Result<Statement<'a>> {
        Ok(Statement::While(while_stmt))
    }

    fn visit_if_statement(&mut self, if_stmt: IfStatement<'a>) -> Result<Statement<'a>> {
        Ok(Statement::If(if_stmt))
    }

    fn visit_return_statement(&mut self, ret_stmt: ReturnStatement<'a>) -> Result<Statement<'a>> {
        Ok(Statement::Return(ret_stmt))
    }

    fn visit_expression(&mut self, expr: Expression<'a>) -> Result<Expression<'a>> {
        match expr {
            Expression::BinaryOperator(binop) => {
                let left = self.visit_expression(*binop.left)?;
                let right = self.visit_expression(*binop.right)?;

                self.visit_binop(BinaryOperatorExpr {
                    op_type: binop.op_type,
                    left: Box::new(left),
                    right: Box::new(right),
                    type_id: binop.type_id,
                    range: binop.range,
                })
            }
            Expression::UnaryOperator(unop) => {
                let expr = self.visit_expression(*unop.expr)?;

                self.visit_unop(UnaryOperatorExpr {
                    op_type: unop.op_type,
                    expr: Box::new(expr),
                    type_id: unop.type_id,
                    range: unop.range,
                })
            }
            Expression::FunctionCall(func_call) => {
                let args = func_call
                    .args
                    .into_iter()
                    .map(|x| self.visit_expression(x))
                    .collect::<Result<_>>()?;

                self.visit_function_call(FunctionCallExpr {
                    name: func_call.name,
                    args,
                    type_id: func_call.type_id,
                    range: func_call.range,
                })
            }
            Expression::IntegerLiteral(int_lit) => self.visit_integer_literal(int_lit),
            Expression::BooleanLiteral(bool_lit) => self.visit_boolean_literal(bool_lit),
            Expression::VariableRef(var_ref) => self.visit_variable_ref(var_ref),
            Expression::StringLiteral(str_lit) => self.visit_string_literal(str_lit),
            Expression::StructLiteral(struct_lit) => {
                let fields = struct_lit
                    .fields
                    .into_iter()
                    // TODO: get rid of unwrap here
                    .map(|(n, v)| (n, self.visit_expression(v).unwrap()))
                    .collect();

                self.visit_struct_literal(StructLiteralExpr {
                    fields,
                    type_id: struct_lit.type_id,
                    range: struct_lit.range,
                })
            }
            Expression::FieldAccessor(field_accessor) => {
                let expr = Box::new(self.visit_expression(*field_accessor.expr)?);

                self.visit_field_accessor(FieldAccessorExpr {
                    name: field_accessor.name,
                    expr,
                    type_id: field_accessor.type_id,
                    range: field_accessor.range,
                })
            }
            Expression::Widen(widen) => {
                let expr = Box::new(self.visit_expression(*widen.expr)?);

                self.visit_widen(WidenExpr {
                    expr,
                    type_id: widen.type_id,
                    range: widen.range,
                })
            }
            Expression::Cast(cast) => {
                let expr = Box::new(self.visit_expression(*cast.expr)?);

                self.visit_cast(CastExpr {
                    expr,
                    type_id: cast.type_id,
                    range: cast.range,
                })
            }
            Expression::Type(_) => unreachable!(),
        }
    }

    fn visit_binop(&mut self, binop: BinaryOperatorExpr<'a>) -> Result<Expression<'a>> {
        Ok(Expression::BinaryOperator(binop))
    }

    fn visit_unop(&mut self, unop: UnaryOperatorExpr<'a>) -> Result<Expression<'a>> {
        Ok(Expression::UnaryOperator(unop))
    }

    fn visit_function_call(&mut self, func_call: FunctionCallExpr<'a>) -> Result<Expression<'a>> {
        Ok(Expression::FunctionCall(func_call))
    }

    fn visit_integer_literal(&mut self, int_lit: IntegerLiteralExpr) -> Result<Expression<'a>> {
        Ok(Expression::IntegerLiteral(int_lit))
    }

    fn visit_boolean_literal(&mut self, bool_lit: BooleanLiteralExpr) -> Result<Expression<'a>> {
        Ok(Expression::BooleanLiteral(bool_lit))
    }

    fn visit_variable_ref(&mut self, var_ref: VariableRefExpr<'a>) -> Result<Expression<'a>> {
        Ok(Expression::VariableRef(var_ref))
    }

    fn visit_string_literal(&mut self, str_lit: StringLiteralExpr<'a>) -> Result<Expression<'a>> {
        Ok(Expression::StringLiteral(str_lit))
    }

    fn visit_struct_literal(
        &mut self,
        struct_lit: StructLiteralExpr<'a>,
    ) -> Result<Expression<'a>> {
        Ok(Expression::StructLiteral(struct_lit))
    }

    fn visit_field_accessor(
        &mut self,
        field_accessor: FieldAccessorExpr<'a>,
    ) -> Result<Expression<'a>> {
        Ok(Expression::FieldAccessor(field_accessor))
    }

    fn visit_widen(&mut self, widen: WidenExpr<'a>) -> Result<Expression<'a>> {
        Ok(Expression::Widen(widen))
    }

    fn visit_cast(&mut self, cast: CastExpr<'a>) -> Result<Expression<'a>> {
        Ok(Expression::Cast(cast))
    }
}
