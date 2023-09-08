use std::{
    cmp::Ordering,
    collections::{HashMap, VecDeque},
    fmt::Display,
};

use crate::{
    ast::{
        AssignmentStatement, BinaryOperatorExpr, BinaryOperatorType, BlockStatement,
        BooleanLiteralExpr, CastExpr, ConstDeclaration, DeclarationStatement, Expression,
        ExpressionStatement, ExternDeclaration, FieldAccessorExpr, FunctionCallExpr,
        FunctionDeclaration, IfStatement, IntegerLiteralExpr, ReturnStatement, Statement,
        StringLiteralExpr, StructDeclaration, StructLiteralExpr, TypeExpr, UnaryOperatorExpr,
        UnaryOperatorType, UpperStatement, VariableRefExpr, WhileStatement, WidenExpr,
    },
    error::{Error, ErrorType, Result},
    parser::{ParsedExpression, ParsedStatement, ParsedType, ParsedUpperStatement},
    scope::Scope,
    types::{FunctionType, StructType, Type, TypeId},
};

pub const BUILTIN_TYPE_UNKNOWN: TypeId = 999999;
pub const BUILTIN_TYPE_VOID: TypeId = 0;
pub const BUILTIN_TYPE_U8: TypeId = 1;
pub const BUILTIN_TYPE_U16: TypeId = 2;
pub const BUILTIN_TYPE_U32: TypeId = 3;
pub const BUILTIN_TYPE_U64: TypeId = 4;
pub const BUILTIN_TYPE_BOOL: TypeId = 5;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct DeclarationId(usize);

impl Display for DeclarationId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "d{}", self.0)
    }
}

pub struct Sema<'a> {
    function_declarations: HashMap<&'a str, FunctionType>,
    declarations: Vec<TypeId>,
    types: Vec<Type>,
    scope: Scope<DeclarationId>,
    current_function_return_type: TypeId,
}

impl<'a> Default for Sema<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a, 'b> Sema<'a> {
    pub fn new() -> Self {
        Self {
            function_declarations: HashMap::new(),
            declarations: Vec::new(),
            types: vec![
                Type::Void(),
                Type::UInt8(),
                Type::UInt16(),
                Type::UInt32(),
                Type::UInt64(),
                Type::Bool(),
            ],
            scope: Scope::new(),
            current_function_return_type: BUILTIN_TYPE_UNKNOWN,
        }
    }

    fn register_type(&mut self, t: Type) -> TypeId {
        self.types.push(t);
        self.types.len() - 1
    }

    fn get_type_id(&mut self, name: &str) -> Result<TypeId> {
        match name {
            "void" => Ok(BUILTIN_TYPE_VOID),
            "u8" => Ok(BUILTIN_TYPE_U8),
            "u16" => Ok(BUILTIN_TYPE_U16),
            "u32" => Ok(BUILTIN_TYPE_U32),
            "u64" => Ok(BUILTIN_TYPE_U64),
            "bool" => Ok(BUILTIN_TYPE_BOOL),
            _ => self
                .types
                .iter()
                .position(|x| matches!(x, Type::Struct(s) if s.name == name))
                .ok_or_else(|| {
                    Error::new(ErrorType::TypeCheck, format!("Could not find type {name}"))
                }),
        }
    }

    pub fn get_type(&self, id: TypeId) -> Result<&Type> {
        self.types.get(id).ok_or_else(|| {
            Error::new(
                ErrorType::TypeCheck,
                format!("Could not find type with id {id}"),
            )
        })
    }

    fn find_or_add_type(&mut self, t: Type) -> TypeId {
        match self.types.iter().position(|x| x == &t) {
            Some(t) => t,
            None => self.register_type(t),
        }
    }

    pub fn get_type_size(&self, id: TypeId) -> Result<usize> {
        let value_type = self.get_type(id)?;

        Ok(match value_type {
            Type::UInt8() => 8,
            Type::UInt16() => 16,
            Type::UInt32() => 32,
            Type::UInt64() => 64,
            Type::Bool() => 8,
            Type::Ptr(_) => 64,
            Type::Void() => unreachable!(),
            Type::Struct(s) => s
                .fields
                .iter()
                .map(|(_, t)| self.get_type_size(*t))
                .collect::<Result<Vec<_>>>()?
                .iter()
                .sum(),
        })
    }

    pub fn add_declaration(&mut self, type_id: TypeId) -> DeclarationId {
        self.declarations.push(type_id);
        DeclarationId(self.declarations.len() - 1)
    }

    pub fn get_declaration_type(&self, declaration_id: DeclarationId) -> TypeId {
        self.declarations[declaration_id.0]
    }

    pub fn get_struct(&self, id: TypeId) -> Result<&StructType> {
        match self.get_type(id)? {
            Type::Struct(s) => Ok(s),
            _ => Err(Error::new(
                ErrorType::TypeCheck,
                format!("Type with id {id} is not a struct"),
            )),
        }
    }

    fn check_type(&mut self, parsed_type: &ParsedType) -> Result<TypeId> {
        match parsed_type {
            ParsedType::Named(name, range) => {
                self.get_type_id(name).map_err(|e| e.with_range(*range))
            }
            ParsedType::Ptr(inner, range) => {
                let inner = self.check_type(inner).map_err(|e| e.with_range(*range))?;
                Ok(self.find_or_add_type(Type::Ptr(inner)))
            }
        }
    }

    fn attempt_register_upper_statement(
        &mut self,
        statement: &ParsedUpperStatement<'a>,
    ) -> Result<()> {
        match statement {
            ParsedUpperStatement::Function {
                name,
                parameters,
                return_type,
                body: _,
                annotations: _,
                range: _,
            } => {
                let parameters = parameters
                    .iter()
                    .map(|(_, t)| self.check_type(t))
                    .collect::<Result<Vec<_>>>()?;

                let return_type = self.check_type(return_type)?;

                self.function_declarations.insert(
                    name,
                    FunctionType {
                        parameters,
                        return_type,
                    },
                );
            }
            ParsedUpperStatement::StructDeclaration {
                name,
                fields,
                range: _,
            } => {
                let checked_fields = fields
                    .iter()
                    .map(|(name, parsed_type)| match self.check_type(parsed_type) {
                        Ok(checked_type) => Ok((name, checked_type)),
                        Err(e) => Err(e),
                    })
                    .collect::<Result<Vec<_>>>()?;

                self.register_type(Type::Struct(StructType {
                    name: name.to_string(),
                    fields: checked_fields
                        .iter()
                        .map(|(x, y)| (x.to_string(), *y))
                        .collect::<Vec<_>>(),
                }));
            }
            _ => {}
        }

        Ok(())
    }

    fn register_upper_statements(&mut self, statements: &[ParsedUpperStatement<'a>]) -> Result<()> {
        let mut pending_statements = VecDeque::new();

        for statement in statements {
            pending_statements.push_back(statement);
        }

        // TODO: on error, this loop will be infinite
        while let Some(statement) = pending_statements.pop_front() {
            if self.attempt_register_upper_statement(statement).is_err() {
                pending_statements.push_back(statement);
            }
        }

        Ok(())
    }

    fn widen_assignment(&self, left: TypeId, right: TypeId) -> Result<Option<TypeId>> {
        let left_size = self.get_type_size(left)?;
        let right_size = self.get_type_size(right)?;

        if left_size < right_size {
            Ok(None)
        } else {
            Ok(Some(left))
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

                let declaration_id = self.add_declaration(type_id);

                self.scope
                    .insert(name, declaration_id)
                    .map_err(|x| x.with_range(range))?;

                Ok(Statement::Declaration(DeclarationStatement {
                    declaration_id,
                    range,
                }))
            }
            ParsedStatement::Assignment { left, right, range } => {
                let left = self.check_expression(left)?;
                let mut right = self.check_expression(right)?;

                if left.type_id() == right.type_id() {
                    return Ok(Statement::Assignment(AssignmentStatement {
                        left,
                        right,
                        range,
                    }));
                }

                let new_type = self
                    .widen_assignment(left.type_id(), right.type_id())?
                    .ok_or_else(|| {
                        Error::new_with_range(
                            ErrorType::TypeCheck,
                            format!(
                                "Cannot widen from type {:?} to {:?}",
                                right.type_id(),
                                left.type_id(),
                            ),
                            range,
                        )
                    })?;

                if new_type != right.type_id() {
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

                if condition.type_id() != BUILTIN_TYPE_BOOL {
                    return Err(Error::new_with_range(
                        ErrorType::TypeCheck,
                        format!(
                            "Condition of if statement should be a boolean but is {:?}",
                            condition.type_id()
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
                if condition.type_id() != BUILTIN_TYPE_BOOL {
                    return Err(Error::new_with_range(
                        ErrorType::TypeCheck,
                        format!(
                            "Condition of while statement should be a boolean but is {:?}",
                            condition.type_id()
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
                let value_type = value.type_id();

                assert_ne!(self.current_function_return_type, BUILTIN_TYPE_UNKNOWN);

                let actual_type =
                    self.widen_assignment(self.current_function_return_type, value_type)?;

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
                let left_type = left.type_id();
                let left_size = self.get_type_size(left_type)?;

                let mut right = self.check_expression(*right)?;
                let right_type = right.type_id();
                let right_size = self.get_type_size(right_type)?;

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
                if self.get_type(left_type)?.is_ptr() && !op_type.is_comparison() {
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

                assert_eq!(left.type_id(), right.type_id());

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
                let expr_type = expr.type_id();

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
                        type_id: self.find_or_add_type(Type::Ptr(expr_type)),
                        range,
                    })),
                    UnaryOperatorType::Deref => Ok(Expression::UnaryOperator(UnaryOperatorExpr {
                        op_type: UnaryOperatorType::Deref,
                        expr: Box::new(expr),
                        type_id: self.get_type(expr_type)?.get_deref()?,
                        range,
                    })),
                }
            }
            ParsedExpression::FunctionCall {
                name,
                arguments: args,
                range,
            } => {
                let function_type =
                    self.function_declarations
                        .get(name)
                        .cloned()
                        .ok_or_else(|| {
                            Error::new_with_range(
                                ErrorType::TypeCheck,
                                format!("Fucntion {name} not found"),
                                range,
                            )
                        })?;

                let return_type = function_type.return_type;

                // NOTE: BUILTIN_TYPE_UNKNOWN is only used for extern functions
                if return_type == BUILTIN_TYPE_UNKNOWN {
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

                assert_eq!(args.len(), function_type.parameters.len());

                let mut new_args = vec![];

                for (arg, expected_type) in args.into_iter().zip(function_type.parameters.iter()) {
                    let arg = self.check_expression(arg)?;
                    let arg_type = arg.type_id();

                    let new_type = self
                        .widen_assignment(*expected_type, arg_type)?
                        .ok_or_else(|| {
                            Error::new_with_range(
                                ErrorType::TypeCheck,
                                format!("Cannot widen from type {arg_type:?} to {expected_type:?}"),
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
                    format!("Unknown intrinsic function '{name}'"),
                    range,
                )),
            },
            ParsedExpression::IntegerLiteral { value, range } => Ok(Expression::IntegerLiteral(
                IntegerLiteralExpr::new(value, range),
            )),
            ParsedExpression::BooleanLiteral { value, range } => {
                Ok(Expression::BooleanLiteral(BooleanLiteralExpr {
                    value,
                    type_id: BUILTIN_TYPE_BOOL,
                    range,
                }))
            }
            ParsedExpression::VariableRef { name, range } => {
                let declaration_id = self.scope.find(name)?;
                let type_id = self.declarations[declaration_id.0];

                Ok(Expression::VariableRef(VariableRefExpr {
                    name,
                    type_id,
                    declaration_id,
                    range,
                }))
            }
            ParsedExpression::StructLiteral {
                struct_type,
                fields,
                range,
            } => {
                let struct_type = self.check_type(&struct_type)?;
                if !self.get_type(struct_type)?.is_struct() {
                    return Err(Error::new_with_range(
                        ErrorType::TypeCheck,
                        "Trying to use struct constant for non-struct type".to_string(),
                        range,
                    ));
                }

                // TODO: this scope feels somewhat stupid
                {
                    let struct_data = self.get_struct(struct_type)?;

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
                }

                let mut checked_fields = vec![];

                for (field_name, field_value) in fields {
                    let mut checked_value = self.check_expression(field_value)?;

                    let struct_data = self.get_struct(struct_type)?;
                    let struct_field = struct_data
                        .fields
                        .iter()
                        .find(|x| x.0 == field_name)
                        .unwrap();

                    let new_type = self
                        .widen_assignment(struct_field.1, checked_value.type_id())?
                        .ok_or_else(|| {
                            Error::new_with_range(
                                ErrorType::TypeCheck,
                                format!(
                                    "Cannot widen from type {:?} to {:?}",
                                    checked_value.type_id(),
                                    struct_field.1,
                                ),
                                range,
                            )
                        })?;

                    if new_type != checked_value.type_id() {
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
                let child_type_id = child.type_id();
                let child_type = self.get_type(child_type_id)?;

                if !child_type.is_struct() && !child_type.is_ptr() {
                    return Err(Error::new_with_range(
                        ErrorType::TypeCheck,
                        format!("Trying to access field '{name}' of non-struct type"),
                        range,
                    ));
                }

                let struct_type = if child_type.is_struct() {
                    self.get_struct(child_type_id)?
                } else {
                    self.get_struct(child_type.get_deref()?)?
                };

                let field_type = struct_type
                    .fields
                    .iter()
                    .find(|(n, _)| n == name)
                    .map(|(_, t)| t);

                match field_type {
                    Some(field_type) => Ok(Expression::FieldAccessor(FieldAccessorExpr {
                        name,
                        expr: Box::new(child),
                        type_id: *field_type,
                        range,
                    })),
                    None => Err(Error::new_with_range(
                        ErrorType::TypeCheck,
                        format!("No field '{name}' exists on type {}", child.type_id()),
                        range,
                    )),
                }
            }
            ParsedExpression::ArrayAccessor { expr, index, range } => {
                let expr = self.check_expression(*expr)?;

                let expr_type = self.get_type(expr.type_id())?;

                if !expr_type.is_ptr() {
                    return Err(Error::new_with_range(
                        ErrorType::TypeCheck,
                        "Cannot user array accessor of non-pointer type".to_string(),
                        range,
                    ));
                }

                let inner_type = expr_type.get_deref()?;

                let index = Expression::Widen(WidenExpr {
                    expr: Box::new(self.check_expression(*index)?),
                    type_id: BUILTIN_TYPE_U64,
                    range,
                });
                let index_type = index.type_id();
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
                    type_id: self.find_or_add_type(Type::Ptr(BUILTIN_TYPE_U8)),
                    range,
                }))
            }
            ParsedExpression::Type { value, range } => {
                let type_id = self.check_type(&value)?;

                Ok(Expression::Type(TypeExpr { type_id, range }))
            }
        }
    }

    fn typecheck_upper_statement(
        &mut self,
        statement: ParsedUpperStatement<'a>,
    ) -> Result<UpperStatement<'a>> {
        match statement {
            ParsedUpperStatement::Function {
                name,
                parameters,
                return_type: _,
                body,
                annotations,
                range,
            } => {
                let function_declaration = self
                    .function_declarations
                    .get(name)
                    .cloned()
                    .ok_or_else(|| {
                        Error::new_with_range(
                            ErrorType::TypeCheck,
                            format!("Fucntion {name} not found"),
                            range,
                        )
                    })?;

                assert_eq!(self.current_function_return_type, BUILTIN_TYPE_UNKNOWN);

                self.current_function_return_type = function_declaration.return_type;

                self.scope.push();

                let mut params = vec![];
                for ((name, _), param_type) in parameters
                    .iter()
                    .zip(function_declaration.parameters.iter())
                {
                    let declaration_id = self.add_declaration(*param_type);

                    self.scope
                        .insert(name, declaration_id)
                        .map_err(|x| x.with_range(range))?;

                    params.push(declaration_id)
                }

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
                    return_type: function_declaration.return_type,
                    body: checked_body,
                    annotations: checked_annotations,
                    range,
                }))
            }
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
                let type_id = self.find_or_add_type(complete_type);

                let declaration_id = self.add_declaration(type_id);

                Ok(UpperStatement::StructDeclaration(StructDeclaration {
                    name,
                    declaration_id,
                    fields: checked_fields,
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
                    .widen_assignment(type_id, value.type_id())?
                    .ok_or_else(|| {
                        Error::new_with_range(
                            ErrorType::TypeCheck,
                            format!(
                                "Cannot widen const type {:?} to {:?}",
                                value.type_id(),
                                type_id
                            ),
                            range,
                        )
                    })?;

                if new_type != value.type_id() {
                    value = Expression::Widen(WidenExpr {
                        expr: Box::new(value),
                        type_id: new_type,
                        range,
                    });
                }

                self.declarations.push(type_id);
                let declaration_id = DeclarationId(self.declarations.len() - 1);

                self.scope.insert(name, declaration_id)?;

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
                    declaration_id,
                    value,
                    annotations: checked_annotations,
                    range,
                }))
            }
            ParsedUpperStatement::ExternDeclaration { name, range } => {
                self.function_declarations.insert(
                    name,
                    FunctionType {
                        parameters: vec![],
                        return_type: BUILTIN_TYPE_UNKNOWN,
                    },
                );

                Ok(UpperStatement::ExternDeclaration(ExternDeclaration {
                    name,
                    range,
                }))
            }
        }
    }

    pub fn analyse(
        &mut self,
        statements: Vec<ParsedUpperStatement<'a>>,
    ) -> Result<Vec<UpperStatement<'a>>> {
        self.register_upper_statements(&statements)?;

        statements
            .into_iter()
            .map(|stmt| self.typecheck_upper_statement(stmt))
            .collect()
    }
}
