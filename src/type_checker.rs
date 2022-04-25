use std::cmp::Ordering;

use crate::{
    ast::{Expression, Statement, UnaryOperatorType},
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

    pub fn check(&mut self, ast: &mut Statement, expected_return_type: &Type) -> Result<()> {
        match ast {
            Statement::Block(statements, scoped, range) => {
                if *scoped {
                    self.scope.push();
                }
                for statement in statements {
                    self.check(statement, expected_return_type)?;
                }
                if *scoped {
                    self.scope.pop(range)?;
                }
            }
            Statement::Declaration(name, variable_type, range) => {
                self.scope
                    .insert(name, TypeScopeEntry::Value(variable_type.clone()), range)?;
            }
            Statement::Assignment(name, expr, range) => {
                let source_type = self.get_type(expr)?;
                let destination_type = self.get_scope_value_type(name, range)?;

                let new_type =
                    Self::widen_assignment(&destination_type, &source_type).ok_or_else(|| {
                        Error::new(
                            ErrorType::TypeCheck,
                            format!(
                                "Cannot widen from type {:?} to {:?}",
                                source_type, destination_type
                            ),
                            range.clone(),
                            self.source_file.to_string(),
                        )
                    })?;

                // TODO: get rid of this clone
                if new_type != source_type {
                    *expr = Expression::Widen(Box::new(expr.clone()), new_type, range.clone());
                }
            }
            Statement::Expression(expr, _range) => {
                self.get_type(expr)?;
            }
            Statement::Function(name, args, return_type, body, range) => {
                self.scope.insert(
                    name,
                    TypeScopeEntry::Function(
                        args.iter().map(|x| x.1.clone()).collect::<Vec<_>>(),
                        return_type.clone(),
                    ),
                    range,
                )?;
                self.scope.push();
                for (arg_name, arg_type) in args {
                    self.scope
                        .insert(arg_name, TypeScopeEntry::Value(arg_type.clone()), range)?;
                }

                self.check(body, return_type)?;

                self.scope.pop(range)?;
            }
            Statement::If(cond, body, range) => {
                let condition_type = self.get_type(cond)?;
                if condition_type != Type::Bool() {
                    return Err(Error::new(
                        ErrorType::TypeCheck,
                        format!(
                            "Condition of if statement should be a boolean but is {:?}",
                            condition_type
                        ),
                        range.clone(),
                        self.source_file.to_string(),
                    ));
                }
                self.check(body, expected_return_type)?;
            }
            Statement::While(cond, body, range) => {
                let condition_type = self.get_type(cond)?;
                if condition_type != Type::Bool() {
                    return Err(Error::new(
                        ErrorType::TypeCheck,
                        format!(
                            "Condition of while statement should be a boolean but is {:?}",
                            condition_type
                        ),
                        range.clone(),
                        self.source_file.to_string(),
                    ));
                }
                self.check(body, expected_return_type)?;
            }
            Statement::Return(expr, range) => {
                let actual_return_type = self.get_type(expr)?;

                let new_actual_type =
                    Self::widen_assignment(expected_return_type, &actual_return_type);

                match new_actual_type {
                    None => {
                        return Err(Error::new(
                            ErrorType::TypeCheck,
                            format!(
                            "Cannot assign value to return type, expected '{:?}' but got '{:?}'",
                            expected_return_type, actual_return_type
                        ),
                            range.clone(),
                            self.source_file.to_string(),
                        ))
                    }
                    Some(t) => {
                        if t != actual_return_type {
                            *expr = Expression::Widen(Box::new(expr.clone()), t, range.clone());
                        }
                    }
                }
            }
        }

        Ok(())
    }

    fn widen_assignment(left: &Type, right: &Type) -> Option<Type> {
        if left.size() < right.size() {
            None
        } else {
            Some(left.clone())
        }
    }

    pub fn get_type(&self, expr: &mut Expression) -> Result<Type> {
        use Expression::*;
        Ok(match expr {
            BinaryOperator(op, left, right, range) => {
                let left_type = self.get_type(left)?;
                let right_type = self.get_type(right)?;

                // TODO: this needs a thorough rework, comparing references won't work
                if left_type.is_ref() && !op.is_comparison() {
                    // TODO: limit this to only addition and subtraction
                    *right = Box::new(Widen(
                        Box::new(*right.clone()),
                        left_type.clone(),
                        range.clone(),
                    ));
                    return Ok(left_type);
                }

                let mut result_type;

                match left_type.size().cmp(&right_type.size()) {
                    Ordering::Greater => {
                        // TODO: clean up this mess
                        *right = Box::new(Widen(
                            Box::new(*right.clone()),
                            left_type.clone(),
                            range.clone(),
                        ));
                        result_type = left_type;
                    }
                    Ordering::Less => {
                        *left = Box::new(Widen(
                            Box::new(*left.clone()),
                            right_type.clone(),
                            range.clone(),
                        ));
                        result_type = right_type;
                    }
                    Ordering::Equal => {
                        result_type = right_type;
                    }
                }

                if op.is_comparison() {
                    result_type = Type::Bool();
                }

                result_type
            }
            UnaryOperator(op, expr, _range) => match op {
                UnaryOperatorType::Negate => self.get_type(expr)?,
                UnaryOperatorType::Ref => self.get_type(expr)?.get_ref(),
                UnaryOperatorType::Deref => self.get_type(expr)?.get_deref(),
            },
            FunctionCall(name, args, ast_return_type, range) => {
                if name == "printf" || name == "exit" || name == "syscall" {
                    for arg in args {
                        self.get_type(arg)?;
                    }
                    *ast_return_type = Type::UInt64();
                    Type::UInt64()
                } else {
                    let (arg_types, return_type) = self.get_scope_function_type(name, range)?;

                    assert_eq!(args.len(), arg_types.len());

                    for (arg_expr, expected_type) in args.iter_mut().zip(arg_types.iter()) {
                        let source_type = self.get_type(arg_expr)?;

                        let new_type = Self::widen_assignment(expected_type, &source_type)
                            .ok_or_else(|| {
                                Error::new(
                                    ErrorType::TypeCheck,
                                    format!(
                                        "Cannot widen from type {:?} to {:?}",
                                        source_type, expected_type
                                    ),
                                    0..0,
                                    self.source_file.to_string(),
                                )
                            })?;

                        if new_type != source_type {
                            *arg_expr = Expression::Widen(
                                Box::new(arg_expr.clone()),
                                new_type,
                                range.clone(),
                            );
                        }
                    }
                    
                    *ast_return_type = return_type.clone();
                    return_type
                }
            }
            Literal(_value, value_type, _range) => value_type.clone(),
            VariableRef(name, range) => self.get_scope_value_type(name, range)?,
            StringLiteral(_, _range) => Type::UInt8().get_ref(),
            Widen(_, value_type, _range) => value_type.clone(),
        })
    }
}