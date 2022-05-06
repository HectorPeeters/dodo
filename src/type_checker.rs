use std::cmp::Ordering;

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

#[derive(Clone)]
pub enum TypeScopeEntry {
    Value(Type),
    Function(Vec<Type>, Type),
}

pub struct TypeChecker<'a> {
    source_file: &'a str,
    scope: Scope<'a, TypeScopeEntry>,
    current_function_return_type: Type,
}

impl<'a> TypeChecker<'a> {
    pub fn new(source_file: &'a str) -> Self {
        Self {
            source_file,
            scope: Scope::new(source_file),
            current_function_return_type: Type::Unknown(),
        }
    }

    fn get_scope_value_type(&self, name: &str, range: &SourceRange) -> Result<Type> {
        use TypeScopeEntry::*;
        match self.scope.find(name, range)? {
            Value(t) => Ok(t),
            Function(_, _) => {
                unreachable!("Trying to get type of value with the name of a function")
            }
        }
    }

    fn get_scope_function_type(
        &self,
        name: &str,
        range: &SourceRange,
    ) -> Result<(Vec<Type>, Type)> {
        use TypeScopeEntry::*;
        match self.scope.find(name, range)? {
            Value(_) => unreachable!("Trying to get type of function with the name of a value"),
            Function(arg_types, return_type) => Ok((arg_types, return_type)),
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

impl<'a> AstTransformer<(), Type> for TypeChecker<'a> {
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
                    self.scope.pop(&range)?;
                }

                Ok(Statement::Block(children, scoped, Type::Unknown(), range))
            }
            Statement::Declaration(name, variable_type, _, range) => {
                self.scope
                    .insert(&name, TypeScopeEntry::Value(variable_type.clone()), &range)?;

                Ok(Statement::Declaration(
                    name.clone(),
                    variable_type.clone(),
                    variable_type,
                    range,
                ))
            }
            Statement::Assignment(name, expr, _, range) => {
                let mut expr = self.transform_expression(expr)?;
                let destination_type = self.get_scope_value_type(&name, &range)?;

                let new_type =
                    Self::widen_assignment(&destination_type, expr.data()).ok_or_else(|| {
                        Error::new(
                            ErrorType::TypeCheck,
                            format!(
                                "Cannot widen from type {:?} to {:?}",
                                expr.data(), destination_type
                            ),
                            range,
                            self.source_file.to_string(),
                        )
                    })?;

                if &new_type != expr.data() {
                    expr = Expression::Widen(Box::new(expr), new_type.clone(), range);
                }

                Ok(Statement::Assignment(name, expr, new_type, range))
            }
            Statement::Expression(expr, _, range) => {
                let expr = self.transform_expression(expr)?;
                let expr_type = expr.data().clone();

                Ok(Statement::Expression(expr, expr_type, range))
            }
            Statement::Function(name, args, return_type, body, _, range) => {
                self.scope.insert(
                    &name,
                    TypeScopeEntry::Function(
                        args.iter().map(|(_, t)| t.clone()).collect::<Vec<_>>(),
                        return_type.clone(),
                    ),
                    &range,
                )?;

                self.scope.push();
                for (arg_name, arg_type) in &args {
                    self.scope
                        .insert(arg_name, TypeScopeEntry::Value(arg_type.clone()), &range)?;
                }

                assert_eq!(self.current_function_return_type, Type::Unknown());
                self.current_function_return_type = return_type.clone();

                let checked_body = self.transform_statement(*body)?;

                self.current_function_return_type = Type::Unknown();

                self.scope.pop(&range)?;

                Ok(Statement::Function(
                    name.clone(),
                    args.clone(),
                    return_type.clone(),
                    Box::new(checked_body),
                    return_type,
                    range,
                ))
            }
            Statement::If(cond, body, _, range) => {
                let cond = self.transform_expression(cond)?;

                if cond.data() != &Type::Bool() {
                    return Err(Error::new(
                        ErrorType::TypeCheck,
                        format!(
                            "Condition of if statement should be a boolean but is {:?}",
                            cond.data()
                        ),
                        range,
                        self.source_file.to_string(),
                    ));
                }
                let body = self.transform_statement(*body)?;
                let body_type = body.data().clone();

                Ok(Statement::If(cond, Box::new(body), body_type, range))
            }
            Statement::While(cond, body, _, range) => {
                let cond = self.transform_expression(cond)?;
                if cond.data() != &Type::Bool() {
                    return Err(Error::new(
                        ErrorType::TypeCheck,
                        format!(
                            "Condition of while statement should be a boolean but is {:?}",
                            cond.data()
                        ),
                        range,
                        self.source_file.to_string(),
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
                    None => Err(Error::new(
                        ErrorType::TypeCheck,
                        format!(
                            "Cannot assign value to return type, expected '{:?}' but got '{:?}'",
                            self.current_function_return_type, expr_type,
                        ),
                        range,
                        self.source_file.to_string(),
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
                    return Err(Error::new(
                        ErrorType::TypeCheck,
                        "Modulo of two 8 bit integers is currently not supported".to_string(),
                        range,
                        self.source_file.to_string(),
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
                if name == "printf" || name == "exit" || name == "syscall" {
                    Ok(FunctionCall(
                        name,
                        args.into_iter()
                            .map(|arg| self.transform_expression(arg))
                            .collect::<Result<Vec<_>>>()?,
                        Type::UInt64(),
                        range,
                    ))
                } else {
                    let (arg_types, return_type) = self.get_scope_function_type(&name, &range)?;

                    assert_eq!(args.len(), arg_types.len());

                    let mut new_args: Vec<TypedExpression> = vec![];

                    for (arg, expected_type) in args.into_iter().zip(arg_types.iter()) {
                        let arg = self.transform_expression(arg)?;
                        let arg_type = arg.data().clone();

                        let new_type = Self::widen_assignment(expected_type, &arg_type)
                            .ok_or_else(|| {
                                Error::new(
                                    ErrorType::TypeCheck,
                                    format!(
                                        "Cannot widen from type {:?} to {:?}",
                                        arg_type, expected_type
                                    ),
                                    range,
                                    self.source_file.to_string(),
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
            }
            Literal(value, _, range) => {
                if value <= 255 {
                    Ok(Literal(value, Type::UInt8(), range))
                } else if value <= 65535 {
                    Ok(Literal(value, Type::UInt16(), range))
                } else if value <= 4294967295 {
                    Ok(Literal(value, Type::UInt32(), range))
                } else {
                    Ok(Literal(value, Type::UInt64(), range))
                }
            }
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
        let mut type_checker = TypeChecker::new("test.dodo");

        type_checker.transform_statement(Statement::Declaration(
            "test".to_string(),
            Type::UInt16(),
            (),
            (0..0).into(),
        ))?;

        let ast = Statement::Assignment(
            "test".to_string(),
            Expression::Literal(12, (), (0..0).into()),
            (),
            (0..0).into(),
        );

        assert_eq!(
            type_checker.transform_statement(ast)?,
            Statement::Assignment(
                "test".to_string(),
                Expression::Widen(
                    Box::new(Expression::Literal(12, Type::UInt8(), (0..0).into())),
                    Type::UInt16(),
                    (0..0).into()
                ),
                Type::UInt16(),
                (0..0).into(),
            )
        );

        Ok(())
    }
}
