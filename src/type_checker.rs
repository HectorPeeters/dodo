use crate::ast::UpperStatement;
use crate::{
    ast::{
        AstTransformer, BinaryOperatorType, Expression, Statement, TypedExpression, TypedStatement,
        UnaryOperatorType,
    },
    error::{Error, ErrorType, Result},
    scope::Scope,
    tokenizer::SourceRange,
    types::Type,
};
use std::cmp::Ordering;

#[derive(Clone)]
pub enum TypeScopeEntry {
    Value(Type),
    Function(Vec<Type>, Type),
    Global(Type),
    ExternalFuction(),
}

pub struct TypeChecker {
    scope: Scope<TypeScopeEntry>,
    current_function_return_type: Type,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            scope: Scope::new(),
            current_function_return_type: Type::Unknown(),
        }
    }

    fn get_scope_value_type(&self, name: &str, range: &SourceRange) -> Result<Type> {
        use TypeScopeEntry::*;
        match self.scope.find(name).map_err(|x| x.with_range(*range))? {
            Value(t) => Ok(t),
            Function(_, _) => {
                unreachable!("Trying to get type of value with the name of a function")
            }
            Global(t) => Ok(t),
            ExternalFuction() => unreachable!(),
        }
    }

    fn get_scope_function_type(
        &self,
        name: &str,
        range: &SourceRange,
    ) -> Result<(Vec<Type>, Type)> {
        use TypeScopeEntry::*;
        match self.scope.find(name).map_err(|x| x.with_range(*range))? {
            Value(_) => unreachable!("Trying to get type of function with the name of a value"),
            Function(arg_types, return_type) => Ok((arg_types, return_type)),
            Global(_) => {
                unreachable!("Trying to get type of value with the name of a global")
            }
            ExternalFuction() => unreachable!(),
        }
    }

    fn is_external_function(&self, name: &str, range: &SourceRange) -> Result<bool> {
        use TypeScopeEntry::*;
        match self.scope.find(name).map_err(|x| x.with_range(*range))? {
            ExternalFuction() => Ok(true),
            _ => Ok(false),
        }
    }

    fn widen_assignment(left: &Type, right: &Type) -> Option<Type> {
        if left.size() < right.size() {
            None
        } else {
            Some(left.clone())
        }
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

impl AstTransformer<(), Type> for TypeChecker {
    fn transform_upper_statement(
        &mut self,
        statement: UpperStatement<()>,
    ) -> Result<UpperStatement<Type>> {
        match statement {
            UpperStatement::ExternDeclaration(name, range) => {
                self.scope
                    .insert(&name, TypeScopeEntry::ExternalFuction())?;
                Ok(UpperStatement::ExternDeclaration(name, range))
            }
            UpperStatement::Function(name, args, return_type, body, annotations, range) => {
                self.scope
                    .insert(
                        &name,
                        TypeScopeEntry::Function(
                            args.iter().map(|(_, t)| t.clone()).collect::<Vec<_>>(),
                            return_type.clone(),
                        ),
                    )
                    .map_err(|x| x.with_range(range))?;

                self.scope.push();
                for (arg_name, arg_type) in &args {
                    self.scope
                        .insert(arg_name, TypeScopeEntry::Value(arg_type.clone()))
                        .map_err(|x| x.with_range(range))?;
                }

                assert_eq!(self.current_function_return_type, Type::Unknown());
                self.current_function_return_type = return_type.clone();

                let checked_body = self.transform_statement(*body)?;

                self.current_function_return_type = Type::Unknown();

                self.scope.pop();

                let checked_annotations = annotations
                    .into_iter()
                    .map(|(name, value)| {
                        if let Some(value) = value {
                            match self.transform_expression(value) {
                                Ok(value) => Ok((name, Some(value))),
                                Err(error) => Err(error),
                            }
                        } else {
                            Ok((name, None))
                        }
                    })
                    .collect::<Result<Vec<_>>>()?;

                Ok(UpperStatement::Function(
                    name.clone(),
                    args,
                    return_type,
                    Box::new(checked_body),
                    checked_annotations,
                    range,
                ))
            }
            UpperStatement::ConstDeclaration(name, value_type, expr, range) => {
                let expr = self.transform_expression(expr)?;

                Self::widen_assignment(&value_type, expr.data()).ok_or_else(|| {
                    Error::new_with_range(
                        ErrorType::TypeCheck,
                        format!(
                            "Cannot widen const type {:?} to {:?}",
                            expr.data(),
                            value_type
                        ),
                        range,
                    )
                })?;

                self.scope
                    .insert(&name, TypeScopeEntry::Global(value_type.clone()))?;

                Ok(UpperStatement::ConstDeclaration(
                    name, value_type, expr, range,
                ))
            }
        }
    }

    fn transform_statement(&mut self, statement: Statement<()>) -> Result<TypedStatement> {
        match statement {
            Statement::Block(statements, scoped, _, range) => {
                if scoped {
                    self.scope.push();
                }

                let children = statements
                    .into_iter()
                    .map(|x| self.transform_statement(x))
                    .collect::<Result<Vec<_>>>()?;

                if scoped {
                    self.scope.pop();
                }

                Ok(Statement::Block(children, scoped, Type::Unknown(), range))
            }
            Statement::Declaration(name, variable_type, _, range) => {
                self.scope
                    .insert(&name, TypeScopeEntry::Value(variable_type.clone()))
                    .map_err(|x| x.with_range(range))?;

                Ok(Statement::Declaration(
                    name.clone(),
                    variable_type.clone(),
                    variable_type,
                    range,
                ))
            }
            Statement::Assignment(lhs, rhs, _, range) => {
                let lhs_checked = self.transform_expression(lhs)?;
                let mut rhs_checked = self.transform_expression(rhs)?;

                if lhs_checked.data() == rhs_checked.data() {
                    let resulting_type = lhs_checked.data().clone();
                    return Ok(Statement::Assignment(
                        lhs_checked,
                        rhs_checked,
                        resulting_type,
                        range,
                    ));
                }

                let new_type = Self::widen_assignment(lhs_checked.data(), rhs_checked.data())
                    .ok_or_else(|| {
                        Error::new_with_range(
                            ErrorType::TypeCheck,
                            format!(
                                "Cannot widen from type {:?} to {:?}",
                                rhs_checked.data(),
                                lhs_checked.data(),
                            ),
                            range,
                        )
                    })?;

                if &new_type != rhs_checked.data() {
                    rhs_checked = Expression::Widen(Box::new(rhs_checked), new_type.clone(), range);
                }

                Ok(Statement::Assignment(
                    lhs_checked,
                    rhs_checked,
                    new_type,
                    range,
                ))
            }
            Statement::Expression(expr, _, range) => {
                let expr = self.transform_expression(expr)?;
                let expr_type = expr.data().clone();

                Ok(Statement::Expression(expr, expr_type, range))
            }
            Statement::If(cond, body, _, range) => {
                let cond = self.transform_expression(cond)?;

                if cond.data() != &Type::Bool() {
                    return Err(Error::new_with_range(
                        ErrorType::TypeCheck,
                        format!(
                            "Condition of if statement should be a boolean but is {:?}",
                            cond.data()
                        ),
                        range,
                    ));
                }
                let body = self.transform_statement(*body)?;
                let body_type = body.data().clone();

                Ok(Statement::If(cond, Box::new(body), body_type, range))
            }
            Statement::While(cond, body, _, range) => {
                let cond = self.transform_expression(cond)?;
                if cond.data() != &Type::Bool() {
                    return Err(Error::new_with_range(
                        ErrorType::TypeCheck,
                        format!(
                            "Condition of while statement should be a boolean but is {:?}",
                            cond.data()
                        ),
                        range,
                    ));
                }

                let body = self.transform_statement(*body)?;
                let body_type = body.data().clone();

                Ok(Statement::While(cond, Box::new(body), body_type, range))
            }
            Statement::Return(expr, _, range) => {
                let mut expr = self.transform_expression(expr)?;
                let expr_type = expr.data().clone();

                assert_ne!(self.current_function_return_type, Type::Unknown());
                let actual_type =
                    Self::widen_assignment(&self.current_function_return_type, &expr_type);

                match actual_type {
                    None => Err(Error::new_with_range(
                        ErrorType::TypeCheck,
                        format!(
                            "Cannot assign value to return type, expected '{:?}' but got '{:?}'",
                            self.current_function_return_type, expr_type,
                        ),
                        range,
                    )),
                    Some(t) => {
                        if t != expr_type {
                            expr = Expression::Widen(Box::new(expr), t, range);
                        }

                        Ok(Statement::Return(expr, expr_type, range))
                    }
                }
            }
        }
    }

    fn transform_expression(&mut self, expression: Expression<()>) -> Result<TypedExpression> {
        use Expression::*;

        match expression {
            BinaryOperator(op, left, right, _, range) => {
                let mut left = self.transform_expression(*left)?;
                let left_type = left.data().clone();
                let mut right = self.transform_expression(*right)?;
                let right_type = right.data().clone();

                // NOTE: We currently don't support this operation as for the eight bit variant,
                // the remainder gets stored in the ah register instead of dl. When we can output
                // assembly using the higher half eight bit registers, we can add this
                // functionality.
                if left_type.size() == 8
                    && right_type.size() == 8
                    && op == BinaryOperatorType::Modulo
                {
                    return Err(Error::new_with_range(
                        ErrorType::TypeCheck,
                        "Modulo of two 8 bit integers is currently not supported".to_string(),
                        range,
                    ));
                }

                // TODO: this needs a thorough rework, comparing references won't work
                if left_type.is_ref() && !op.is_comparison() {
                    // TODO: limit this to only addition and subtraction
                    return Ok(BinaryOperator(
                        op,
                        Box::new(left),
                        Box::new(Widen(Box::new(right), left_type.clone(), range)),
                        left_type,
                        range,
                    ));
                }

                match left_type.size().cmp(&right_type.size()) {
                    Ordering::Greater => {
                        right = Widen(Box::new(right), left_type.clone(), range);
                    }
                    Ordering::Less => {
                        left = Widen(Box::new(left), right_type, range);
                    }
                    _ => {}
                };

                let result_type = if op.is_comparison() {
                    Type::Bool()
                } else {
                    left_type
                };

                Ok(BinaryOperator(
                    op,
                    Box::new(left),
                    Box::new(right),
                    result_type,
                    range,
                ))
            }
            UnaryOperator(op, expr, _, range) => {
                let expr = self.transform_expression(*expr)?;
                let expr_type = expr.data().clone();

                match op {
                    UnaryOperatorType::Negate => Ok(UnaryOperator(
                        UnaryOperatorType::Negate,
                        Box::new(expr),
                        expr_type,
                        range,
                    )),
                    UnaryOperatorType::Ref => Ok(UnaryOperator(
                        UnaryOperatorType::Ref,
                        Box::new(expr),
                        expr_type.get_ref(),
                        range,
                    )),
                    UnaryOperatorType::Deref => Ok(UnaryOperator(
                        UnaryOperatorType::Deref,
                        Box::new(expr),
                        expr_type.get_deref(),
                        range,
                    )),
                }
            }
            FunctionCall(name, args, _, range) => {
                if self.is_external_function(&name, &range)? {
                    return Ok(FunctionCall(
                        name,
                        args.into_iter()
                            .map(|arg| self.transform_expression(arg))
                            .collect::<Result<Vec<_>>>()?,
                        Type::UInt64(),
                        range,
                    ));
                }

                let (arg_types, return_type) = self.get_scope_function_type(&name, &range)?;

                assert_eq!(args.len(), arg_types.len());

                let mut new_args: Vec<TypedExpression> = vec![];

                for (arg, expected_type) in args.into_iter().zip(arg_types.iter()) {
                    let arg = self.transform_expression(arg)?;
                    let arg_type = arg.data().clone();

                    let new_type =
                        Self::widen_assignment(expected_type, &arg_type).ok_or_else(|| {
                            Error::new_with_range(
                                ErrorType::TypeCheck,
                                format!(
                                    "Cannot widen from type {:?} to {:?}",
                                    arg_type, expected_type
                                ),
                                range,
                            )
                        })?;

                    if new_type != arg_type {
                        new_args.push(Expression::Widen(Box::new(arg), new_type, range));
                    } else {
                        new_args.push(arg);
                    }
                }

                Ok(FunctionCall(name, new_args, return_type, range))
            }
            IntegerLiteral(value, _, range) => {
                if value <= 255 {
                    Ok(IntegerLiteral(value, Type::UInt8(), range))
                } else if value <= 65535 {
                    Ok(IntegerLiteral(value, Type::UInt16(), range))
                } else if value <= 4294967295 {
                    Ok(IntegerLiteral(value, Type::UInt32(), range))
                } else {
                    Ok(IntegerLiteral(value, Type::UInt64(), range))
                }
            }
            BooleanLiteral(value, _, range) => Ok(BooleanLiteral(value, Type::Bool(), range)),
            VariableRef(name, _, range) => {
                let value_type = self.get_scope_value_type(&name, &range)?;

                Ok(VariableRef(name, value_type, range))
            }
            StringLiteral(value, _, range) => {
                Ok(StringLiteral(value, Type::UInt8().get_ref(), range))
            }
            Widen(_expr, _value_type, _range) => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::Result;

    #[test]
    fn widen_assignment() -> Result<()> {
        let mut type_checker = TypeChecker::new();

        type_checker.transform_statement(Statement::Declaration(
            "test".to_string(),
            Type::UInt16(),
            (),
            (0..0).into(),
        ))?;

        let ast = Statement::Assignment(
            Expression::VariableRef("test".to_string(), (), (0..0).into()),
            Expression::IntegerLiteral(12, (), (0..0).into()),
            (),
            (0..0).into(),
        );

        assert_eq!(
            type_checker.transform_statement(ast)?,
            Statement::Assignment(
                Expression::VariableRef("test".to_string(), Type::UInt16(), (0..0).into()),
                Expression::Widen(
                    Box::new(Expression::IntegerLiteral(12, Type::UInt8(), (0..0).into())),
                    Type::UInt16(),
                    (0..0).into(),
                ),
                Type::UInt16(),
                (0..0).into(),
            )
        );

        Ok(())
    }
}
