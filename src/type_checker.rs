use crate::ast::{
    AssignmentStatement, BinaryOperatorExpr, BlockStatement, BooleanLiteralExpr, CastExpr,
    ConstDeclaration, DeclarationStatement, ExpressionStatement, ExternDeclaration,
    FieldAccessorExpr, FunctionCallExpr, FunctionDeclaration, IfStatement, IntegerLiteralExpr,
    ReturnStatement, StringLiteralExpr, StructDeclaration, StructLiteralExpr, TypeExpr,
    UnaryOperatorExpr, UpperStatement, VariableRefExpr, WhileStatement, WidenExpr,
};
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

impl<'a, 'b> TypeChecker<'a> {
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
        statement: ParsedUpperStatement<'b>,
    ) -> Result<UpperStatement<'b>> {
        match statement {
            ParsedUpperStatement::StructDeclaration {
                name,
                fields,
                range,
            } => {
                let checked_fields = fields
                    .into_iter()
                    .map(|(name, parsed_type)| match self.check_type(&parsed_type) {
                        Ok(checked_type) => Ok((name, checked_type)),
                        Err(e) => Err(e),
                    })
                    .collect::<Result<Vec<_>>>()?;

                let complete_type = Type::Struct(StructType {
                    name: name.to_string(),
                    fields: checked_fields
                        .iter()
                        .map(|(x, y)| (x.to_string(), *y))
                        .collect::<Vec<_>>(),
                });
                self.project.find_or_add_type(complete_type);

                Ok(UpperStatement::StructDeclaration(StructDeclaration {
                    name,
                    fields: checked_fields,
                    range,
                }))
            }
            ParsedUpperStatement::ExternDeclaration { name, range } => {
                self.scope.insert(name, TypeScopeEntry::ExternalFuction())?;
                Ok(UpperStatement::ExternDeclaration(ExternDeclaration {
                    name,
                    range,
                }))
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
                        name,
                        TypeScopeEntry::Function(checked_parameters, checked_return_type),
                    )
                    .map_err(|x| x.with_range(range))?;

                self.scope.push();

                let params = parameters
                    .into_iter()
                    .map(
                        |(param_name, param_type)| match self.check_type(&param_type) {
                            Ok(param_type) => Ok((param_name, param_type)),
                            Err(e) => Err(e),
                        },
                    )
                    .collect::<Result<Vec<_>>>()?;

                for (param_name, param_type) in &params {
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

                Ok(UpperStatement::Function(FunctionDeclaration {
                    name,
                    params,
                    return_type,
                    body: checked_body,
                    annotations: checked_annotations,
                    range,
                }))
            }
            ParsedUpperStatement::ConstDeclaration {
                name,
                value_type,
                value,
                annotations,
                range,
            } => {
                let mut value = self.check_expression(value)?;

                let type_id = self.check_type(&value_type)?;

                let new_type = self
                    .widen_assignment(type_id, value.get_type())
                    .ok_or_else(|| {
                        Error::new_with_range(
                            ErrorType::TypeCheck,
                            format!(
                                "Cannot widen const type {:?} to {:?}",
                                value.get_type(),
                                type_id
                            ),
                            range,
                        )
                    })?;

                if new_type != value.get_type() {
                    value = Expression::Widen(WidenExpr {
                        expr: Box::new(value),
                        type_id: new_type,
                        range,
                    });
                }

                self.scope.insert(name, TypeScopeEntry::Global(type_id))?;

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

                Ok(UpperStatement::ConstDeclaration(ConstDeclaration {
                    name,
                    type_id,
                    value,
                    annotations: checked_annotations,
                    range,
                }))
            }
        }
    }

    fn check_statement(&mut self, statement: ParsedStatement<'b>) -> Result<Statement<'b>> {
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

                Ok(Statement::Block(BlockStatement {
                    children,
                    scoped,
                    range,
                }))
            }
            ParsedStatement::Declaration {
                name,
                value_type,
                range,
            } => {
                let type_id = self.check_type(&value_type)?;

                self.scope
                    .insert(name, TypeScopeEntry::Value(type_id))
                    .map_err(|x| x.with_range(range))?;

                Ok(Statement::Declaration(DeclarationStatement {
                    name,
                    type_id,
                    range,
                }))
            }
            ParsedStatement::Assignment { left, right, range } => {
                let left = self.check_expression(left)?;
                let mut right = self.check_expression(right)?;

                if left.get_type() == right.get_type() {
                    return Ok(Statement::Assignment(AssignmentStatement {
                        left,
                        right,
                        range,
                    }));
                }

                let new_type = self
                    .widen_assignment(left.get_type(), right.get_type())
                    .ok_or_else(|| {
                        Error::new_with_range(
                            ErrorType::TypeCheck,
                            format!(
                                "Cannot widen from type {:?} to {:?}",
                                right.get_type(),
                                left.get_type(),
                            ),
                            range,
                        )
                    })?;

                if new_type != right.get_type() {
                    right = Expression::Widen(WidenExpr {
                        expr: Box::new(right),
                        type_id: new_type,
                        range,
                    });
                }

                Ok(Statement::Assignment(AssignmentStatement {
                    left,
                    right,
                    range,
                }))
            }
            ParsedStatement::Expression { expr, range } => {
                let expr = self.check_expression(expr)?;

                Ok(Statement::Expression(ExpressionStatement { expr, range }))
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

                let if_body = Box::new(self.check_statement(*body)?);

                let else_body = match else_body {
                    Some(b) => Some(Box::new(self.check_statement(*b)?)),
                    None => None,
                };

                Ok(Statement::If(IfStatement {
                    condition,
                    if_body,
                    else_body,
                    range,
                }))
            }
            ParsedStatement::While {
                condition,
                body,
                range,
            } => {
                let condition = self.check_expression(condition)?;
                if condition.get_type() != BUILTIN_TYPE_BOOL {
                    return Err(Error::new_with_range(
                        ErrorType::TypeCheck,
                        format!(
                            "Condition of while statement should be a boolean but is {:?}",
                            condition.get_type()
                        ),
                        range,
                    ));
                }

                let body = Box::new(self.check_statement(*body)?);

                Ok(Statement::While(WhileStatement {
                    condition,
                    body,
                    range,
                }))
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
                    Some(type_id) => {
                        if type_id != value_type {
                            value = Expression::Widen(WidenExpr {
                                expr: Box::new(value),
                                type_id,
                                range,
                            });
                        }

                        Ok(Statement::Return(ReturnStatement { expr: value, range }))
                    }
                }
            }
        }
    }

    fn check_expression(&mut self, expression: ParsedExpression<'b>) -> Result<Expression<'b>> {
        match expression {
            ParsedExpression::BinaryOperator {
                op_type,
                left,
                right,
                range,
            } => {
                // TODO: add support for logic operators
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
                    return Ok(Expression::BinaryOperator(BinaryOperatorExpr {
                        op_type,
                        left: Box::new(left),
                        right: Box::new(Expression::Widen(WidenExpr {
                            expr: Box::new(right),
                            type_id: left_type,
                            range,
                        })),
                        type_id: left_type,
                        range,
                    }));
                }

                let result_type = match left_size.cmp(&right_size) {
                    Ordering::Greater => {
                        right = Expression::Widen(WidenExpr {
                            expr: Box::new(right),
                            type_id: left_type,
                            range,
                        });
                        left_type
                    }
                    Ordering::Less => {
                        left = Expression::Widen(WidenExpr {
                            expr: Box::new(left),
                            type_id: right_type,
                            range,
                        });
                        right_type
                    }
                    _ => left_type,
                };

                assert_eq!(left.get_type(), right.get_type());

                let result_type = if op_type.is_comparison() {
                    BUILTIN_TYPE_BOOL
                } else {
                    result_type
                };

                Ok(Expression::BinaryOperator(BinaryOperatorExpr {
                    op_type,
                    left: Box::new(left),
                    right: Box::new(right),
                    type_id: result_type,
                    range,
                }))
            }
            ParsedExpression::UnaryOperator {
                op_type,
                expr,
                range,
            } => {
                let expr = self.check_expression(*expr)?;
                let expr_type = expr.get_type();

                match op_type {
                    UnaryOperatorType::Negate => Ok(Expression::UnaryOperator(UnaryOperatorExpr {
                        op_type: UnaryOperatorType::Negate,
                        expr: Box::new(expr),
                        type_id: expr_type,
                        range,
                    })),
                    UnaryOperatorType::Ref => Ok(Expression::UnaryOperator(UnaryOperatorExpr {
                        op_type: UnaryOperatorType::Ref,
                        expr: Box::new(expr),
                        type_id: self.project.find_or_add_type(Type::Ptr(expr_type)),
                        range,
                    })),
                    UnaryOperatorType::Deref => Ok(Expression::UnaryOperator(UnaryOperatorExpr {
                        op_type: UnaryOperatorType::Deref,
                        expr: Box::new(expr),
                        type_id: self.project.get_inner_type(expr_type),
                        range,
                    })),
                }
            }
            ParsedExpression::FunctionCall {
                name,
                arguments: args,
                range,
            } => {
                if self.is_external_function(name, &range)? {
                    return Ok(Expression::FunctionCall(FunctionCallExpr {
                        name,
                        args: args
                            .into_iter()
                            .map(|arg| self.check_expression(arg))
                            .collect::<Result<Vec<_>>>()?,
                        type_id: BUILTIN_TYPE_U64,
                        range,
                    }));
                }

                let (arg_types, return_type) = self.get_scope_function_type(name, &range)?;

                assert_eq!(args.len(), arg_types.len());

                let mut new_args = vec![];

                for (arg, expected_type) in args.into_iter().zip(arg_types.iter()) {
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
                        new_args.push(Expression::Widen(WidenExpr {
                            expr: Box::new(arg),
                            type_id: new_type,
                            range,
                        }));
                    } else {
                        new_args.push(arg);
                    }
                }

                Ok(Expression::FunctionCall(FunctionCallExpr {
                    name,
                    args: new_args,
                    type_id: return_type,
                    range,
                }))
            }
            ParsedExpression::IntrinsicCall {
                name,
                mut arguments,
                range,
            } => match name {
                "cast" => {
                    if arguments.len() != 2 {
                        return Err(Error::new_with_range(
                            ErrorType::TypeCheck,
                            "Intrinsic function 'cast' expects two arguments".to_string(),
                            range,
                        ));
                    }

                    let checked_argument = self.check_expression(arguments.remove(0))?;

                    let target_type = self.check_expression(arguments.remove(0))?;

                    if let Expression::Type(expr_type) = target_type {
                        Ok(Expression::Cast(CastExpr {
                            expr: Box::new(checked_argument),
                            type_id: expr_type.type_id,
                            range,
                        }))
                    } else {
                        Err(Error::new_with_range(
                            ErrorType::TypeCheck,
                            "Second argument of 'cast' is not a type".to_string(),
                            *target_type.range(),
                        ))
                    }
                }
                _ => Err(Error::new_with_range(
                    ErrorType::TypeCheck,
                    format!("Unknown intrinsic function '{}'", name),
                    range,
                )),
            },
            ParsedExpression::IntegerLiteral { value, range } => {
                let type_id = if value <= 255 {
                    BUILTIN_TYPE_U8
                } else if value <= 65535 {
                    BUILTIN_TYPE_U16
                } else if value <= 4294967295 {
                    BUILTIN_TYPE_U32
                } else {
                    BUILTIN_TYPE_U64
                };

                Ok(Expression::IntegerLiteral(IntegerLiteralExpr {
                    value,
                    type_id,
                    range,
                }))
            }
            ParsedExpression::BooleanLiteral { value, range } => {
                Ok(Expression::BooleanLiteral(BooleanLiteralExpr {
                    value,
                    type_id: BUILTIN_TYPE_BOOL,
                    range,
                }))
            }
            ParsedExpression::VariableRef { name, range } => {
                let type_id = self.get_scope_value_type(name, &range)?;

                Ok(Expression::VariableRef(VariableRefExpr {
                    name,
                    type_id,
                    range,
                }))
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
                    if !fields.iter().any(|f| f.0 == expected_name) {
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
                        checked_value = Expression::Widen(WidenExpr {
                            expr: Box::new(checked_value),
                            type_id: new_type,
                            range,
                        });
                    }

                    checked_fields.push((field_name, checked_value));
                }

                Ok(Expression::StructLiteral(StructLiteralExpr {
                    fields: checked_fields,
                    type_id: struct_type,
                    range,
                }))
            }
            ParsedExpression::FieldAccessor { child, name, range } => {
                let child = self.check_expression(*child)?;
                let child_type = child.get_type();

                let is_struct = self.project.is_struct_type(child_type);
                if is_struct {
                    let struct_type = self.project.get_struct(child.get_type());

                    let field_type = struct_type
                        .fields
                        .iter()
                        .find(|(n, _)| n == name)
                        .map(|(_, t)| t);

                    return match field_type {
                        Some(field_type) => Ok(Expression::FieldAccessor(FieldAccessorExpr {
                            name,
                            expr: Box::new(child),
                            type_id: *field_type,
                            range,
                        })),
                        None => Err(Error::new_with_range(
                            ErrorType::TypeCheck,
                            format!("No field '{name}' exists on type {}", child.get_type()),
                            range,
                        )),
                    };
                }

                let is_struct_pointer = self
                    .project
                    .is_struct_type(self.project.get_inner_type(child_type));

                if is_struct_pointer {
                    let struct_type = self
                        .project
                        .get_struct(self.project.get_inner_type(child.get_type()));
                    let field_type = struct_type
                        .fields
                        .iter()
                        .find(|(n, _)| n == name)
                        .map(|(_, t)| t);

                    return match field_type {
                        Some(field_type) => Ok(Expression::FieldAccessor(FieldAccessorExpr {
                            name,
                            expr: Box::new(child),
                            type_id: *field_type,
                            range,
                        })),
                        None => Err(Error::new_with_range(
                            ErrorType::TypeCheck,
                            format!("No field '{name}' exists on type {}", child.get_type()),
                            range,
                        )),
                    };
                }

                Err(Error::new_with_range(
                    ErrorType::TypeCheck,
                    format!("Trying to access field '{name}' of non-struct type"),
                    range,
                ))
            }
            ParsedExpression::ArrayAccessor { expr, index, range } => {
                let expr = self.check_expression(*expr)?;

                if !self.project.is_ptr_type(expr.get_type()) {
                    return Err(Error::new_with_range(
                        ErrorType::TypeCheck,
                        "Cannot user array accessor of non-pointer type".to_string(),
                        range,
                    ));
                }

                let inner_type = self.project.get_inner_type(expr.get_type());

                let index = Expression::Widen(WidenExpr {
                    expr: Box::new(self.check_expression(*index)?),
                    type_id: BUILTIN_TYPE_U64,
                    range,
                });
                let index_type = index.get_type();
                let index_range = *index.range();

                // Convert $expr[$index] into *($expr + $index)
                Ok(Expression::UnaryOperator(UnaryOperatorExpr {
                    op_type: UnaryOperatorType::Deref,
                    expr: Box::new(Expression::BinaryOperator(BinaryOperatorExpr {
                        op_type: BinaryOperatorType::Add,
                        left: Box::new(expr),
                        right: Box::new(index),
                        type_id: index_type,
                        range: index_range,
                    })),
                    type_id: inner_type,
                    range,
                }))
            }
            ParsedExpression::StringLiteral { value, range } => {
                Ok(Expression::StringLiteral(StringLiteralExpr {
                    value,
                    type_id: self.project.find_or_add_type(Type::Ptr(BUILTIN_TYPE_U8)),
                    range,
                }))
            }
            ParsedExpression::Type { value, range } => {
                let type_id = self.check_type(&value)?;

                Ok(Expression::Type(TypeExpr { type_id, range }))
            }
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
            name: "test",
            value_type: ParsedType::Named("u16".to_string()),
            range: (0..0).into(),
        })?;

        let ast = ParsedStatement::Assignment {
            left: ParsedExpression::VariableRef {
                name: "test",
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
            Statement::Assignment(AssignmentStatement {
                left: Expression::VariableRef(VariableRefExpr {
                    name: "test",
                    type_id: BUILTIN_TYPE_U16,
                    range: (0..0).into()
                }),
                right: Expression::Widen(WidenExpr {
                    expr: Box::new(Expression::IntegerLiteral(IntegerLiteralExpr {
                        value: 12,
                        type_id: BUILTIN_TYPE_U8,
                        range: (0..0).into()
                    })),
                    type_id: BUILTIN_TYPE_U16,
                    range: (0..0).into(),
                }),
                range: (0..0).into(),
            })
        );

        Ok(())
    }
}
