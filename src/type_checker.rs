use std::cmp::Ordering;

use crate::{
    ast::{Expression, Statement, TypedExpression, TypedStatement, UnaryOperatorType},
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
}

impl<'a> TypeChecker<'a> {
    pub fn new(source_file: &'a str) -> Self {
        Self {
            source_file,
            scope: Scope::new(source_file),
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

    pub fn check(
        &mut self,
        ast: Statement<()>,
        expected_return_type: &Type,
    ) -> Result<TypedStatement> {
        match ast {
            Statement::Block(statements, scoped, _, range) => {
                if scoped {
                    self.scope.push();
                    // defer self.scope.pop(range)?;
                }
                let children = statements
                    .into_iter()
                    .map(|x| self.check(x, expected_return_type))
                    .collect::<Result<Vec<_>>>()?;
                if scoped {
                    self.scope.pop(&range)?;
                }

                Ok(TypedStatement::Block(children, scoped, Type::Void(), range))
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
                let mut checked_expr = self.check_expression(expr)?;
                let destination_type = self.get_scope_value_type(&name, &range)?;

                let new_type = Self::widen_assignment(&destination_type, checked_expr.data())
                    .ok_or_else(|| {
                        Error::new(
                            ErrorType::TypeCheck,
                            format!(
                                "Cannot widen from type {:?} to {:?}",
                                checked_expr, destination_type
                            ),
                            range.clone(),
                            self.source_file.to_string(),
                        )
                    })?;

                // TODO: get rid of this clone
                if &new_type != checked_expr.data() {
                    checked_expr =
                        Expression::Widen(Box::new(checked_expr), new_type.clone(), range.clone());
                }

                Ok(Statement::Assignment(name, checked_expr, new_type, range))
            }
            Statement::Expression(expr, _, range) => {
                let checked_expr = self.check_expression(expr)?;
                Ok(Statement::Expression(
                    checked_expr.clone(),
                    checked_expr.data().clone(),
                    range,
                ))
            }
            Statement::Function(name, args, return_type, body, _, range) => {
                self.scope.insert(
                    &name,
                    TypeScopeEntry::Function(
                        args.iter().map(|x| x.1.clone()).collect::<Vec<_>>(),
                        return_type.clone(),
                    ),
                    &range,
                )?;
                self.scope.push();
                for (arg_name, arg_type) in &args {
                    self.scope
                        .insert(arg_name, TypeScopeEntry::Value(arg_type.clone()), &range)?;
                }

                let checked_body = self.check(*body, &return_type)?;

                self.scope.pop(&range)?;

                Ok(Statement::Function(
                    name.clone(),
                    args.clone(),
                    return_type.clone(),
                    Box::new(checked_body),
                    return_type.clone(),
                    range,
                ))
            }
            Statement::If(cond, body, _, range) => {
                let checked_cond = self.check_expression(cond)?;
                if checked_cond.data() != &Type::Bool() {
                    return Err(Error::new(
                        ErrorType::TypeCheck,
                        format!(
                            "Condition of if statement should be a boolean but is {:?}",
                            checked_cond.data()
                        ),
                        range,
                        self.source_file.to_string(),
                    ));
                }
                let checked_body = self.check(*body, expected_return_type)?;

                Ok(Statement::If(
                    checked_cond,
                    Box::new(checked_body.clone()),
                    checked_body.data().clone(),
                    range,
                ))
            }
            Statement::While(cond, body, _, range) => {
                let checked_cond = self.check_expression(cond)?;
                if checked_cond.data() != &Type::Bool() {
                    return Err(Error::new(
                        ErrorType::TypeCheck,
                        format!(
                            "Condition of while statement should be a boolean but is {:?}",
                            checked_cond.data()
                        ),
                        range,
                        self.source_file.to_string(),
                    ));
                }

                let checked_body = self.check(*body, expected_return_type)?;

                Ok(Statement::While(
                    checked_cond,
                    Box::new(checked_body.clone()),
                    checked_body.data().clone(),
                    range,
                ))
            }
            Statement::Return(expr, _, range) => {
                let mut checked_expr = self.check_expression(expr)?;

                let new_actual_type =
                    Self::widen_assignment(expected_return_type, checked_expr.data());

                match new_actual_type {
                    None => Err(Error::new(
                        ErrorType::TypeCheck,
                        format!(
                            "Cannot assign value to return type, expected '{:?}' but got '{:?}'",
                            expected_return_type,
                            checked_expr.data()
                        ),
                        range,
                        self.source_file.to_string(),
                    )),
                    Some(t) => {
                        if &t != checked_expr.data() {
                            checked_expr =
                                Expression::Widen(Box::new(checked_expr), t, range.clone());
                        }

                        Ok(Statement::Return(
                            checked_expr.clone(),
                            checked_expr.data().clone(),
                            range,
                        ))
                    }
                }
            }
        }
    }

    fn widen_assignment(left: &Type, right: &Type) -> Option<Type> {
        if left.size() < right.size() {
            None
        } else {
            Some(left.clone())
        }
    }

    pub fn check_expression(&self, expr: Expression<()>) -> Result<TypedExpression> {
        use Expression::*;

        match expr {
            BinaryOperator(op, left, right, _, range) => {
                let mut checked_left = self.check_expression(*left)?;
                let mut checked_right = self.check_expression(*right)?;

                // TODO: this needs a thorough rework, comparing references won't work
                if checked_left.data().is_ref() && !op.is_comparison() {
                    // TODO: limit this to only addition and subtraction
                    return Ok(BinaryOperator(
                        op,
                        Box::new(checked_left.clone()),
                        Box::new(Widen(
                            Box::new(checked_right),
                            checked_left.data().clone(),
                            range.clone(),
                        )),
                        checked_left.data().clone(),
                        range,
                    ));
                }

                let mut result_type =
                    match checked_left.data().size().cmp(&checked_right.data().size()) {
                        Ordering::Greater => {
                            // TODO: clean up this mess
                            checked_right = Widen(
                                Box::new(checked_right),
                                checked_left.data().clone(),
                                range.clone(),
                            );
                            checked_left.data().clone()
                        }
                        Ordering::Less => {
                            checked_left = Widen(
                                Box::new(checked_left),
                                checked_right.data().clone(),
                                range.clone(),
                            );
                            checked_right.data().clone()
                        }
                        Ordering::Equal => checked_right.data().clone(),
                    };

                if op.is_comparison() {
                    result_type = Type::Bool();
                }

                Ok(BinaryOperator(
                    op,
                    Box::new(checked_left),
                    Box::new(checked_right),
                    result_type,
                    range,
                ))
            }
            UnaryOperator(op, expr, _, range) => {
                let checked_expr = self.check_expression(*expr)?;
                match op {
                    UnaryOperatorType::Negate => Ok(UnaryOperator(
                        UnaryOperatorType::Negate,
                        Box::new(checked_expr.clone()),
                        checked_expr.data().clone(),
                        range,
                    )),
                    UnaryOperatorType::Ref => Ok(UnaryOperator(
                        UnaryOperatorType::Ref,
                        Box::new(checked_expr.clone()),
                        checked_expr.data().clone().get_ref(),
                        range,
                    )),
                    UnaryOperatorType::Deref => Ok(UnaryOperator(
                        UnaryOperatorType::Deref,
                        Box::new(checked_expr.clone()),
                        checked_expr.data().clone().get_deref(),
                        range,
                    )),
                }
            }
            FunctionCall(name, args, _, range) => {
                if name == "printf" || name == "exit" || name == "syscall" {
                    Ok(FunctionCall(
                        name,
                        args.into_iter()
                            .map(|arg| self.check_expression(arg))
                            .collect::<Result<Vec<_>>>()?,
                        Type::UInt64(),
                        range,
                    ))
                } else {
                    let (arg_types, return_type) = self.get_scope_function_type(&name, &range)?;

                    assert_eq!(args.len(), arg_types.len());

                    let mut new_args: Vec<TypedExpression> = vec![];

                    for (arg_expr, expected_type) in args.into_iter().zip(arg_types.iter()) {
                        let checked_arg = self.check_expression(arg_expr)?;

                        let new_type = Self::widen_assignment(expected_type, checked_arg.data())
                            .ok_or_else(|| {
                                Error::new(
                                    ErrorType::TypeCheck,
                                    format!(
                                        "Cannot widen from type {:?} to {:?}",
                                        checked_arg.data(),
                                        expected_type
                                    ),
                                    0..0,
                                    self.source_file.to_string(),
                                )
                            })?;

                        if &new_type != checked_arg.data() {
                            new_args.push(Expression::Widen(
                                Box::new(checked_arg),
                                new_type,
                                range.clone(),
                            ));
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
            VariableRef(name, _, range) => Ok(VariableRef(
                name.clone(),
                self.get_scope_value_type(&name, &range)?,
                range,
            )),
            StringLiteral(value, _, range) => {
                Ok(StringLiteral(value, Type::UInt8().get_ref(), range))
            }
            Widen(_expr, _value_type, _range) => unreachable!(),
        }
    }
}
