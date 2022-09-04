use crate::ast::UpperStatement;
use crate::parser::{ParsedExpression, ParsedStatement, ParsedType, ParsedUpperStatement};
use crate::project::{
    Project, BUILTIN_TYPE_BOOL, BUILTIN_TYPE_U16, BUILTIN_TYPE_U32, BUILTIN_TYPE_U64,
    BUILTIN_TYPE_U8, BUILTIN_TYPE_UNKNOWN,
};
use crate::types::{StructType, TypeId};
use crate::{
    ast::{BinaryOperatorType, Expression, Statement, UnaryOperatorType},
    error::{Error, ErrorType, Result},
    scope::Scope,
    tokenizer::SourceRange,
    types::Type,
};
use std::cmp::Ordering;

#[derive(Clone)]
pub enum TypeScopeEntry {
    Value(TypeId),
    Function(Vec<TypeId>, TypeId),
    Global(TypeId),
    ExternalFuction(),
}

pub struct TypeChecker<'a> {
    project: &'a mut Project,
    scope: Scope<TypeScopeEntry>,
    current_function_return_type: TypeId,
}

impl<'a> TypeChecker<'a> {
    pub fn new(project: &'a mut Project) -> Self {
        Self {
            project,
            scope: Scope::new(),
            current_function_return_type: BUILTIN_TYPE_UNKNOWN,
        }
    }

    fn get_scope_value_type(&self, name: &str, range: &SourceRange) -> Result<TypeId> {
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
    ) -> Result<(Vec<TypeId>, TypeId)> {
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

    fn widen_assignment(&self, left: TypeId, right: TypeId) -> Option<TypeId> {
        let left_size = self.project.get_type_size(left);
        let right_size = self.project.get_type_size(right);

        if left_size < right_size {
            None
        } else {
            Some(left)
        }
    }

    fn check_type(&mut self, parsed_type: &ParsedType) -> Result<TypeId> {
        match parsed_type {
            ParsedType::Named(name) => self.project.lookup_builtin_type(name).ok_or_else(|| {
                // TODO: add range
                Error::new(
                    ErrorType::TypeCheck,
                    format!("Could not find type '{}'", name),
                )
            }),
            ParsedType::Ptr(inner) => {
                let inner = self.check_type(inner)?;
                Ok(self.project.find_or_add_type(Type::Ptr(inner)))
            }
        }
    }

    pub fn check_upper_statement(
        &mut self,
        statement: ParsedUpperStatement,
    ) -> Result<UpperStatement> {
        match statement {
            ParsedUpperStatement::StructDeclaration { name, fields } => {
                let checked_fields = fields
                    .into_iter()
                    .map(|(name, parsed_type)| match self.check_type(&parsed_type) {
                        Ok(checked_type) => Ok((name, checked_type)),
                        Err(e) => Err(e),
                    })
                    .collect::<Result<Vec<_>>>()?;

                let complete_type = Type::Struct(StructType {
                    name: name.clone(),
                    fields: checked_fields.clone(),
                });
                self.project.find_or_add_type(complete_type);

                Ok(UpperStatement::StructDeclaratin(name, checked_fields))
            }
            ParsedUpperStatement::ExternDeclaration { name, range } => {
                self.scope
                    .insert(&name, TypeScopeEntry::ExternalFuction())?;
                Ok(UpperStatement::ExternDeclaration(name, range))
            }
            ParsedUpperStatement::Function {
                name,
                parameters,
                return_type,
                body,
                annotations,
                range,
            } => {
                let checked_parameters = parameters
                    .iter()
                    .map(|(_, t)| self.check_type(t))
                    .collect::<Result<Vec<_>>>()?;

                let checked_return_type = self.check_type(&return_type)?;

                self.scope
                    .insert(
                        &name,
                        TypeScopeEntry::Function(checked_parameters, checked_return_type),
                    )
                    .map_err(|x| x.with_range(range))?;

                self.scope.push();

                let parameters = parameters
                    .into_iter()
                    .map(
                        |(param_name, param_type)| match self.check_type(&param_type) {
                            Ok(param_type) => Ok((param_name, param_type)),
                            Err(e) => Err(e),
                        },
                    )
                    .collect::<Result<Vec<_>>>()?;

                for (param_name, param_type) in &parameters {
                    self.scope
                        .insert(param_name, TypeScopeEntry::Value(*param_type))
                        .map_err(|x| x.with_range(range))?;
                }

                assert_eq!(self.current_function_return_type, BUILTIN_TYPE_UNKNOWN);
                let return_type = self.check_type(&return_type)?;
                self.current_function_return_type = return_type;

                let checked_body = self.check_statement(body)?;

                self.current_function_return_type = BUILTIN_TYPE_UNKNOWN;

                self.scope.pop();

                let checked_annotations = annotations
                    .into_iter()
                    .map(|(name, value)| {
                        if let Some(value) = value {
                            match self.check_expression(value) {
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
                    parameters,
                    return_type,
                    Box::new(checked_body),
                    checked_annotations,
                    range,
                ))
            }
            ParsedUpperStatement::ConstDeclaration {
                name,
                value_type,
                value,
                annotations,
                range,
            } => {
                let mut value = self.check_expression(value)?;

                let value_type = self.check_type(&value_type)?;

                let new_type = self
                    .widen_assignment(value_type, value.get_type())
                    .ok_or_else(|| {
                        Error::new_with_range(
                            ErrorType::TypeCheck,
                            format!(
                                "Cannot widen const type {:?} to {:?}",
                                value.get_type(),
                                value_type
                            ),
                            range,
                        )
                    })?;

                if new_type != value.get_type() {
                    value = Expression::Widen(Box::new(value), new_type, range);
                }

                self.scope
                    .insert(&name, TypeScopeEntry::Global(value_type))?;

                let checked_annotations = annotations
                    .into_iter()
                    .map(|(name, value)| {
                        if let Some(value) = value {
                            match self.check_expression(value) {
                                Ok(value) => Ok((name, Some(value))),
                                Err(error) => Err(error),
                            }
                        } else {
                            Ok((name, None))
                        }
                    })
                    .collect::<Result<Vec<_>>>()?;

                Ok(UpperStatement::ConstDeclaration(
                    name,
                    value_type,
                    value,
                    checked_annotations,
                    range,
                ))
            }
        }
    }

    fn check_statement(&mut self, statement: ParsedStatement) -> Result<Statement> {
        match statement {
            ParsedStatement::Block {
                children,
                scoped,
                range,
            } => {
                if scoped {
                    self.scope.push();
                }

                let children = children
                    .into_iter()
                    .map(|x| self.check_statement(x))
                    .collect::<Result<Vec<_>>>()?;

                if scoped {
                    self.scope.pop();
                }

                Ok(Statement::Block(children, scoped, range))
            }
            ParsedStatement::Declaration {
                name,
                value_type,
                range,
            } => {
                let value_type = self.check_type(&value_type)?;

                self.scope
                    .insert(&name, TypeScopeEntry::Value(value_type))
                    .map_err(|x| x.with_range(range))?;

                Ok(Statement::Declaration(name.clone(), value_type, range))
            }
            ParsedStatement::Assignment { left, right, range } => {
                let left_checked = self.check_expression(left)?;
                let mut right_checked = self.check_expression(right)?;

                // TODO: this should probably be moved into another step later on
                if let Expression::StructLiteral(fields, right_type, _) = &right_checked {
                    if left_checked.get_type() != *right_type {
                        return Err(Error::new_with_range(
                            ErrorType::TypeCheck,
                            "Trying to assign struct of incorrect type".to_string(),
                            range,
                        ));
                    }

                    let assign_instructions = fields
                        .iter()
                        .map(|(name, value)| {
                            Statement::Assignment(
                                Expression::FieldAccessor(
                                    name.to_string(),
                                    Box::new(left_checked.clone()),
                                    value.get_type(),
                                    range,
                                ),
                                // TODO: this clone can robably be removed
                                value.clone(),
                                range,
                            )
                        })
                        .collect::<Vec<_>>();

                    return Ok(Statement::Block(assign_instructions, false, range));
                }

                if left_checked.get_type() == right_checked.get_type() {
                    return Ok(Statement::Assignment(left_checked, right_checked, range));
                }

                let new_type = self
                    .widen_assignment(left_checked.get_type(), right_checked.get_type())
                    .ok_or_else(|| {
                        Error::new_with_range(
                            ErrorType::TypeCheck,
                            format!(
                                "Cannot widen from type {:?} to {:?}",
                                right_checked.get_type(),
                                left_checked.get_type(),
                            ),
                            range,
                        )
                    })?;

                if new_type != right_checked.get_type() {
                    right_checked = Expression::Widen(Box::new(right_checked), new_type, range);
                }

                Ok(Statement::Assignment(left_checked, right_checked, range))
            }
            ParsedStatement::Expression { expr, range } => {
                let expr = self.check_expression(expr)?;

                Ok(Statement::Expression(expr, range))
            }
            ParsedStatement::If {
                condition,
                body,
                else_body,
                range,
            } => {
                let condition = self.check_expression(condition)?;

                if condition.get_type() != BUILTIN_TYPE_BOOL {
                    return Err(Error::new_with_range(
                        ErrorType::TypeCheck,
                        format!(
                            "Condition of if statement should be a boolean but is {:?}",
                            condition.get_type()
                        ),
                        range,
                    ));
                }

                let body = self.check_statement(*body)?;

                let else_body = match else_body {
                    Some(b) => Some(Box::new(self.check_statement(*b)?)),
                    None => None,
                };

                Ok(Statement::If(condition, Box::new(body), else_body, range))
            }
            ParsedStatement::While {
                condition,
                body,
                range,
            } => {
                let cond = self.check_expression(condition)?;
                if cond.get_type() != BUILTIN_TYPE_BOOL {
                    return Err(Error::new_with_range(
                        ErrorType::TypeCheck,
                        format!(
                            "Condition of while statement should be a boolean but is {:?}",
                            cond.get_type()
                        ),
                        range,
                    ));
                }

                let body = self.check_statement(*body)?;

                Ok(Statement::While(cond, Box::new(body), range))
            }
            ParsedStatement::Return { value, range } => {
                let mut value = self.check_expression(value)?;
                let value_type = value.get_type();

                assert_ne!(self.current_function_return_type, BUILTIN_TYPE_UNKNOWN);

                let actual_type =
                    self.widen_assignment(self.current_function_return_type, value_type);

                match actual_type {
                    None => Err(Error::new_with_range(
                        ErrorType::TypeCheck,
                        format!(
                            "Cannot assign value to return type, expected '{:?}' but got '{:?}'",
                            self.current_function_return_type, value_type,
                        ),
                        range,
                    )),
                    Some(t) => {
                        if t != value_type {
                            value = Expression::Widen(Box::new(value), t, range);
                        }

                        Ok(Statement::Return(value, range))
                    }
                }
            }
        }
    }

    fn check_expression(&mut self, expression: ParsedExpression) -> Result<Expression> {
        match expression {
            ParsedExpression::BinaryOperator {
                op_type,
                left,
                right,
                range,
            } => {
                let mut left = self.check_expression(*left)?;
                let left_type = left.get_type();
                let left_size = self.project.get_type_size(left_type);

                let mut right = self.check_expression(*right)?;
                let right_type = right.get_type();
                let right_size = self.project.get_type_size(right_type);

                // NOTE: We currently don't support this operation as for the eight bit variant,
                // the remainder gets stored in the ah register instead of dl. When we can output
                // assembly using the higher half eight bit registers, we can add this
                // functionality.
                if left_size == 8 && right_size == 8 && op_type == BinaryOperatorType::Modulo {
                    return Err(Error::new_with_range(
                        ErrorType::TypeCheck,
                        "Modulo of two 8 bit integers is currently not supported".to_string(),
                        range,
                    ));
                }

                // TODO: this needs a thorough rework, comparing references won't work
                if self.project.is_ptr_type(left_type) && !op_type.is_comparison() {
                    // TODO: limit this to only addition and subtraction
                    return Ok(Expression::BinaryOperator(
                        op_type,
                        Box::new(left),
                        Box::new(Expression::Widen(Box::new(right), left_type, range)),
                        left_type,
                        range,
                    ));
                }

                match left_size.cmp(&right_size) {
                    Ordering::Greater => {
                        right = Expression::Widen(Box::new(right), left_type, range);
                    }
                    Ordering::Less => {
                        left = Expression::Widen(Box::new(left), right_type, range);
                    }
                    _ => {}
                };

                let result_type = if op_type.is_comparison() {
                    BUILTIN_TYPE_BOOL
                } else {
                    left_type
                };

                Ok(Expression::BinaryOperator(
                    op_type,
                    Box::new(left),
                    Box::new(right),
                    result_type,
                    range,
                ))
            }
            ParsedExpression::UnaryOperator {
                op_type,
                expr,
                range,
            } => {
                let expr = self.check_expression(*expr)?;
                let expr_type = expr.get_type();

                match op_type {
                    UnaryOperatorType::Negate => Ok(Expression::UnaryOperator(
                        UnaryOperatorType::Negate,
                        Box::new(expr),
                        expr_type,
                        range,
                    )),
                    UnaryOperatorType::Ref => Ok(Expression::UnaryOperator(
                        UnaryOperatorType::Ref,
                        Box::new(expr),
                        self.project.find_or_add_type(Type::Ptr(expr_type)),
                        range,
                    )),
                    UnaryOperatorType::Deref => Ok(Expression::UnaryOperator(
                        UnaryOperatorType::Deref,
                        Box::new(expr),
                        self.project.get_inner_type(expr_type),
                        range,
                    )),
                }
            }
            ParsedExpression::FunctionCall {
                name,
                arguments,
                range,
            } => {
                if self.is_external_function(&name, &range)? {
                    return Ok(Expression::FunctionCall(
                        name,
                        arguments
                            .into_iter()
                            .map(|arg| self.check_expression(arg))
                            .collect::<Result<Vec<_>>>()?,
                        BUILTIN_TYPE_U64,
                        range,
                    ));
                }

                let (arg_types, return_type) = self.get_scope_function_type(&name, &range)?;

                assert_eq!(arguments.len(), arg_types.len());

                let mut new_args = vec![];

                for (arg, expected_type) in arguments.into_iter().zip(arg_types.iter()) {
                    let arg = self.check_expression(arg)?;
                    let arg_type = arg.get_type();

                    let new_type =
                        self.widen_assignment(*expected_type, arg_type)
                            .ok_or_else(|| {
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

                Ok(Expression::FunctionCall(name, new_args, return_type, range))
            }
            ParsedExpression::IntrinsicCall {
                name,
                mut arguments,
                range,
            } => match &name[..] {
                "castU16Ptr" => {
                    if arguments.len() != 1 {
                        return Err(Error::new_with_range(
                            ErrorType::TypeCheck,
                            "Intrinsic function 'castPtrU16' expects only one argument".to_string(),
                            range,
                        ));
                    }

                    let checked_argument = self.check_expression(arguments.remove(0))?;

                    Ok(Expression::Cast(
                        Box::new(checked_argument),
                        self.project.find_or_add_type(Type::Ptr(BUILTIN_TYPE_U16)),
                        range,
                    ))
                }
                _ => Err(Error::new_with_range(
                    ErrorType::TypeCheck,
                    format!("Unknown intrinsic function '{}'", name),
                    range,
                )),
            },
            ParsedExpression::IntegerLiteral { value, range } => {
                if value <= 255 {
                    Ok(Expression::IntegerLiteral(value, BUILTIN_TYPE_U8, range))
                } else if value <= 65535 {
                    Ok(Expression::IntegerLiteral(value, BUILTIN_TYPE_U16, range))
                } else if value <= 4294967295 {
                    Ok(Expression::IntegerLiteral(value, BUILTIN_TYPE_U32, range))
                } else {
                    Ok(Expression::IntegerLiteral(value, BUILTIN_TYPE_U64, range))
                }
            }
            ParsedExpression::BooleanLiteral { value, range } => {
                Ok(Expression::BooleanLiteral(value, BUILTIN_TYPE_BOOL, range))
            }
            ParsedExpression::VariableRef { name, range } => {
                let value_type = self.get_scope_value_type(&name, &range)?;

                Ok(Expression::VariableRef(name, value_type, range))
            }
            ParsedExpression::StructLiteral {
                struct_type,
                fields,
                range,
            } => {
                let struct_type = self.check_type(&struct_type)?;
                if !self.project.is_struct_type(struct_type) {
                    return Err(Error::new_with_range(
                        ErrorType::TypeCheck,
                        "Trying to use struct constant for non-struct type".to_string(),
                        range,
                    ));
                }

                let struct_data = self.project.get_struct(struct_type).clone();

                for (expected_name, _) in &struct_data.fields {
                    if !fields.iter().any(|f| &f.0 == expected_name) {
                        return Err(Error::new_with_range(
                            ErrorType::TypeCheck,
                            format!("Missing field '{expected_name}' in struct constructor"),
                            range,
                        ));
                    }
                }

                if struct_data.fields.len() != fields.len() {
                    return Err(Error::new_with_range(
                        ErrorType::TypeCheck,
                        format!(
                            "Too many fields in struct constructor for '{}'",
                            struct_data.name
                        ),
                        range,
                    ));
                }

                let mut checked_fields = vec![];

                for (field_name, field_value) in fields {
                    let struct_field = struct_data
                        .fields
                        .iter()
                        .find(|x| x.0 == field_name)
                        .unwrap();

                    let mut checked_value = self.check_expression(field_value)?;

                    let new_type = self
                        .widen_assignment(struct_field.1, checked_value.get_type())
                        .ok_or_else(|| {
                            Error::new_with_range(
                                ErrorType::TypeCheck,
                                format!(
                                    "Cannot widen from type {:?} to {:?}",
                                    checked_value.get_type(),
                                    struct_field.1,
                                ),
                                range,
                            )
                        })?;

                    if new_type != checked_value.get_type() {
                        checked_value = Expression::Widen(Box::new(checked_value), new_type, range);
                    }

                    checked_fields.push((field_name, checked_value));
                }

                Ok(Expression::StructLiteral(
                    checked_fields,
                    struct_type,
                    range,
                ))
            }
            ParsedExpression::FieldAccessor { child, name, range } => {
                let child = self.check_expression(*child)?;
                if !self.project.is_struct_type(child.get_type()) {
                    return Err(Error::new_with_range(
                        ErrorType::TypeCheck,
                        format!("Trying to access field '{name}' of non-struct type"),
                        range,
                    ));
                }

                let struct_type = self.project.get_struct(child.get_type());

                let field_type = struct_type
                    .fields
                    .iter()
                    .find(|(n, _)| n == &name)
                    .map(|(_, t)| t);

                if field_type.is_none() {
                    return Err(Error::new_with_range(
                        ErrorType::TypeCheck,
                        format!("No field '{name}' exists on type {}", child.get_type()),
                        range,
                    ));
                }

                let field_type = field_type.unwrap();

                Ok(Expression::FieldAccessor(
                    name,
                    Box::new(child),
                    *field_type,
                    range,
                ))
            }
            ParsedExpression::StringLiteral { value, range } => Ok(Expression::StringLiteral(
                value,
                self.project.find_or_add_type(Type::Ptr(BUILTIN_TYPE_U8)),
                range,
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::Result;

    #[test]
    fn widen_assignment() -> Result<()> {
        let mut project = Project::new("test::widen_assignment");
        let mut type_checker = TypeChecker::new(&mut project);

        type_checker.check_statement(ParsedStatement::Declaration {
            name: "test".to_string(),
            value_type: ParsedType::Named("u16".to_string()),
            range: (0..0).into(),
        })?;

        let ast = ParsedStatement::Assignment {
            left: ParsedExpression::VariableRef {
                name: "test".to_string(),
                range: (0..0).into(),
            },
            right: ParsedExpression::IntegerLiteral {
                value: 12,
                range: (0..0).into(),
            },
            range: (0..0).into(),
        };

        assert_eq!(
            type_checker.check_statement(ast)?,
            Statement::Assignment(
                Expression::VariableRef("test".to_string(), BUILTIN_TYPE_U16, (0..0).into()),
                Expression::Widen(
                    Box::new(Expression::IntegerLiteral(
                        12,
                        BUILTIN_TYPE_U8,
                        (0..0).into()
                    )),
                    BUILTIN_TYPE_U16,
                    (0..0).into(),
                ),
                (0..0).into(),
            )
        );

        Ok(())
    }
}
