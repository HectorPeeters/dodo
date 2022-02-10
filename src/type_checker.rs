use crate::{
    ast::{Expression, Statement},
    error::{Error, ErrorType, Result},
    scope::Scope,
    types::Type,
};

#[derive(Clone)]
pub enum TypeScopeEntry {
    Value(Type),
    Function(Vec<Type>, Type),
}

pub struct TypeChecker<'a> {
    source_file: &'a str,
    scope: Scope<TypeScopeEntry>,
}

impl<'a> TypeChecker<'a> {
    pub fn new(source_file: &'a str) -> Self {
        Self {
            source_file,
            scope: Scope::new(source_file),
        }
    }

    fn get_scope_value_type(&self, name: &str) -> Result<Type> {
        use TypeScopeEntry::*;
        match self.scope.find(name)? {
            Value(t) => Ok(t),
            Function(_, _) => {
                unreachable!("Trying to get type of value with the name of a function")
            }
        }
    }

    fn get_scope_function_type(&self, name: &str) -> Result<(Vec<Type>, Type)> {
        use TypeScopeEntry::*;
        match self.scope.find(name)? {
            Value(_) => unreachable!("Trying to get type of function with the name of a value"),
            Function(arg_types, return_type) => Ok((arg_types, return_type)),
        }
    }

    pub fn check(&mut self, ast: &mut Statement<u64>) -> Result<()> {
        match ast {
            Statement::Block(statements, scoped) => {
                if *scoped {
                    self.scope.push();
                }
                for statement in statements {
                    self.check(statement)?;
                }
                if *scoped {
                    self.scope.pop()?;
                }
            }
            Statement::Declaration(name, variable_type) => {
                self.scope
                    .insert(name, TypeScopeEntry::Value(variable_type.clone()))?;
            }
            Statement::Assignment(name, expr) => {
                let source_type = self.get_type(expr)?;
                let destination_type = self.get_scope_value_type(name)?;

                let new_type =
                    Self::widen_assignment(&destination_type, &source_type).ok_or(Error::new(
                        ErrorType::TypeCheck,
                        format!(
                            "Cannot widen from type {:?} to {:?}",
                            source_type, destination_type
                        ),
                        0..0,
                        self.source_file.to_string(),
                    ))?;

                // TODO: get rid of this clone
                if new_type != source_type {
                    *expr = Expression::Widen(Box::new(expr.clone()), new_type);
                }
            }
            Statement::Expression(expr) => {
                self.get_type(expr)?;
            }
            Statement::Function(name, args, return_type, body) => {
                self.scope.insert(
                    name,
                    TypeScopeEntry::Function(
                        args.iter().map(|x| x.1.clone()).collect::<Vec<_>>(),
                        return_type.clone(),
                    ),
                )?;
                self.scope.push();
                for (arg_name, arg_type) in args {
                    self.scope
                        .insert(arg_name, TypeScopeEntry::Value(arg_type.clone()))?;
                }

                // TODO: check return type
                self.check(body)?;

                self.scope.pop()?;
            }
            _ => unreachable!("{:?}", ast),
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

    pub fn get_type(&self, expr: &mut Expression<u64>) -> Result<Type> {
        use Expression::*;
        Ok(match expr {
            BinaryOperator(_, left, right) => {
                let left_type = self.get_type(left)?;
                let right_type = self.get_type(right)?;
                if left_type.size() > right_type.size() {
                    // TODO: clean up this mess
                    *right = Box::new(Widen(Box::new(*right.clone()), left_type.clone()));
                    left_type
                } else if left_type.size() < right_type.size() {
                    *left = Box::new(Widen(Box::new(*left.clone()), right_type.clone()));
                    right_type
                } else {
                    right_type
                }
            }
            UnaryOperator(_, expr) => self.get_type(expr)?,
            FunctionCall(name, args) => {
                if name == "printf" || name == "exit" {
                    Type::Void()
                } else {
                    let (arg_types, return_type) = self.get_scope_function_type(name)?;

                    assert_eq!(args.len(), arg_types.len());

                    for (mut arg_expr, expected_type) in args.iter_mut().zip(arg_types.iter()) {
                        let source_type = self.get_type(&mut arg_expr)?;

                        let new_type = Self::widen_assignment(&expected_type, &source_type).ok_or(
                            Error::new(
                                ErrorType::TypeCheck,
                                format!(
                                    "Cannot widen from type {:?} to {:?}",
                                    source_type, expected_type
                                ),
                                0..0,
                                self.source_file.to_string(),
                            ),
                        )?;

                        if new_type != source_type {
                            *arg_expr = Expression::Widen(Box::new(arg_expr.clone()), new_type);
                        }
                    }

                    return_type
                }
            }
            Literal(_, value_type) => value_type.clone(),
            VariableRef(name) => self.get_scope_value_type(name)?,
            StringLiteral(_) => Type::UInt64(),
            Widen(_, value_type) => value_type.clone(),
        })
    }
}
