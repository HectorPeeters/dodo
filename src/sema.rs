use std::{
    cmp::Ordering,
    collections::{HashMap, VecDeque},
    fmt::Display,
};

use crate::{
    ast::{
        AssignmentStatement, Ast, BinaryOperatorExpr, BinaryOperatorType, BlockStatement,
        BooleanLiteralExpr, CastExpr, ConstDeclaration, DeclarationStatement, Expression,
        ExpressionId, ExpressionStatement, ExternDeclaration, FieldAccessorExpr, FunctionCallExpr,
        FunctionDeclaration, IfStatement, IntegerLiteralExpr, ReturnStatement, Statement,
        StatementId, StringLiteralExpr, StructDeclaration, StructLiteralExpr, TypeExpr,
        UnaryOperatorExpr, UnaryOperatorType, UpperStatement, UpperStatementId, VariableRefExpr,
        WhileStatement, WidenExpr,
    },
    error::{Error, ErrorType, Result},
    parser::{
        ParsedAst, ParsedExpression, ParsedExpressionId, ParsedStatement, ParsedStatementId,
        ParsedType, ParsedUpperStatement, ParsedUpperStatementId,
    },
    scope::Scope,
    types::{
        FunctionType, StructType, Type, TypeId, BUILTIN_TYPE_BOOL, BUILTIN_TYPE_U16,
        BUILTIN_TYPE_U32, BUILTIN_TYPE_U64, BUILTIN_TYPE_U8, BUILTIN_TYPE_UNKNOWN,
        BUILTIN_TYPE_VOID,
    },
};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct DeclarationId(usize);

impl Display for DeclarationId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "d{}", self.0)
    }
}

pub struct Declaration {
    pub type_id: TypeId,
    pub is_constant: bool,
    pub value: Option<ExpressionId>,
}

pub struct Sema<'a> {
    parsed_ast: &'a ParsedAst<'a>,
    ast: Ast<'a>,
    expression_types: Vec<TypeId>,

    function_declarations: HashMap<&'a str, FunctionType>,
    declarations: Vec<Declaration>,

    types: Vec<Type>,
    scope: Scope<DeclarationId>,

    current_function_return_type: TypeId,
}

impl<'a> Sema<'a> {
    pub fn new(parsed_ast: &'a ParsedAst<'a>) -> Self {
        Self {
            parsed_ast,
            ast: Ast::new(),
            expression_types: Vec::new(),
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

    pub fn get_ast(&self) -> &Ast<'a> {
        &self.ast
    }

    pub fn get_type(&self, expression_id: ExpressionId) -> TypeId {
        self.expression_types[*expression_id as usize]
    }

    pub fn add_expression(
        &mut self,
        expression: Expression<'a>,
        type_id: TypeId,
    ) -> Result<ExpressionId> {
        let expression_id = self.ast.add_expression(expression);
        // NOTE: when ast.add_expression changes, this probably won't work
        self.expression_types.push(type_id);
        Ok(expression_id)
    }

    fn register_type(&mut self, t: Type) -> TypeId {
        self.types.push(t);
        (self.types.len() as u32 - 1).into()
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
                .map(|p| (p as u32).into())
                .ok_or_else(|| {
                    Error::new(ErrorType::TypeCheck, format!("Could not find type {name}"))
                }),
        }
    }

    pub fn get_type_info(&self, id: TypeId) -> Result<&Type> {
        self.types.get(*id as usize).ok_or_else(|| {
            Error::new(
                ErrorType::TypeCheck,
                format!("Could not find type with id {}", *id),
            )
        })
    }

    fn find_or_add_type(&mut self, t: Type) -> TypeId {
        match self.types.iter().position(|x| x == &t) {
            Some(t) => (t as u32).into(),
            None => self.register_type(t),
        }
    }

    pub fn get_type_size(&self, id: TypeId) -> Result<usize> {
        let value_type = self.get_type_info(id)?;

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

    pub fn add_declaration(&mut self, type_id: TypeId, is_constant: bool) -> DeclarationId {
        self.declarations.push(Declaration {
            type_id,
            is_constant,
            value: None,
        });
        DeclarationId(self.declarations.len() - 1)
    }

    pub fn get_declaration(&self, declaration_id: DeclarationId) -> &Declaration {
        &self.declarations[declaration_id.0]
    }

    pub fn add_declaration_value(&mut self, declaration_id: DeclarationId, value: ExpressionId) {
        self.declarations[declaration_id.0].value = Some(value);
    }

    pub fn get_struct(&self, id: TypeId) -> Result<&StructType> {
        match self.get_type_info(id)? {
            Type::Struct(s) => Ok(s),
            _ => Err(Error::new(
                ErrorType::TypeCheck,
                format!("Type with id {} is not a struct", *id),
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
                range,
            } => {
                let checked_fields = fields
                    .iter()
                    .map(|(name, parsed_type)| match self.check_type(parsed_type) {
                        Ok(checked_type) => Ok((name, checked_type)),
                        Err(e) => Err(e),
                    })
                    .collect::<Result<Vec<_>>>()
                    .map_err(|x| x.with_range(*range))?;

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

    fn check_statement(&mut self, statement_id: ParsedStatementId) -> Result<StatementId> {
        let statement = self.parsed_ast.get_statement(statement_id);
        match statement {
            ParsedStatement::Block {
                children,
                scoped,
                range,
            } => {
                if *scoped {
                    self.scope.push();
                }

                let children_ids = children
                    .iter()
                    .map(|x| self.check_statement(*x))
                    .collect::<Result<Vec<_>>>()?;

                if *scoped {
                    self.scope.pop();
                }

                let statement = Statement::Block(BlockStatement {
                    children_ids,
                    scoped: *scoped,
                    range: *range,
                });
                Ok(self.ast.add_statement(statement))
            }
            ParsedStatement::Declaration {
                name,
                value_type,
                range,
            } => {
                let type_id = self.check_type(value_type)?;

                let declaration_id = self.add_declaration(type_id, false);

                self.scope
                    .insert(name, declaration_id)
                    .map_err(|x| x.with_range(*range))?;

                let statement = Statement::Declaration(DeclarationStatement {
                    declaration_id,
                    range: *range,
                });
                Ok(self.ast.add_statement(statement))
            }
            ParsedStatement::Assignment { left, right, range } => {
                let left_id = self.check_expression(*left)?;
                let mut right_id = self.check_expression(*right)?;

                if self.get_type(left_id) == self.get_type(right_id) {
                    let statement = Statement::Assignment(AssignmentStatement {
                        left_id,
                        right_id,
                        range: *range,
                    });

                    return Ok(self.ast.add_statement(statement));
                }

                let new_type = self
                    .widen_assignment(self.get_type(left_id), self.get_type(right_id))?
                    .ok_or_else(|| {
                        Error::new_with_range(
                            ErrorType::TypeCheck,
                            format!(
                                "Cannot widen from type {} to {}",
                                self.get_type(right_id),
                                self.get_type(left_id)
                            ),
                            *range,
                        )
                    })?;

                if new_type != self.get_type(right_id) {
                    right_id = self.add_expression(
                        Expression::Widen(WidenExpr {
                            expr_id: right_id,
                            range: *range,
                        }),
                        new_type,
                    )?;
                }

                let statement = Statement::Assignment(AssignmentStatement {
                    left_id,
                    right_id,
                    range: *range,
                });
                Ok(self.ast.add_statement(statement))
            }
            ParsedStatement::Expression { expr, range } => {
                let expr_id = self.check_expression(*expr)?;

                let statement = Statement::Expression(ExpressionStatement {
                    expr_id,
                    range: *range,
                });

                Ok(self.ast.add_statement(statement))
            }
            ParsedStatement::If {
                condition,
                body,
                else_body,
                range,
            } => {
                let condition_id = self.check_expression(*condition)?;

                if self.get_type(condition_id) != BUILTIN_TYPE_BOOL {
                    return Err(Error::new_with_range(
                        ErrorType::TypeCheck,
                        format!(
                            "Condition of if statement should be a boolean but is {}",
                            self.get_type(condition_id)
                        ),
                        *range,
                    ));
                }

                let if_body_id = self.check_statement(*body)?;

                let else_body_id = match else_body {
                    Some(b) => Some(self.check_statement(*b)?),
                    None => None,
                };

                let statement = Statement::If(IfStatement {
                    condition_id,
                    if_body_id,
                    else_body_id,
                    range: *range,
                });
                Ok(self.ast.add_statement(statement))
            }
            ParsedStatement::While {
                condition,
                body,
                range,
            } => {
                let condition_id = self.check_expression(*condition)?;

                if self.get_type(condition_id) != BUILTIN_TYPE_BOOL {
                    return Err(Error::new_with_range(
                        ErrorType::TypeCheck,
                        format!(
                            "Condition of while statement should be a boolean but is {}",
                            self.get_type(condition_id)
                        ),
                        *range,
                    ));
                }

                let body_id = self.check_statement(*body)?;

                let statement = Statement::While(WhileStatement {
                    condition_id,
                    body_id,
                    range: *range,
                });
                Ok(self.ast.add_statement(statement))
            }
            ParsedStatement::Return { value, range } => {
                let mut expr_id = self.check_expression(*value)?;
                let expr_type = self.get_type(expr_id);

                assert_ne!(self.current_function_return_type, BUILTIN_TYPE_UNKNOWN);

                let actual_type =
                    self.widen_assignment(self.current_function_return_type, expr_type)?;

                match actual_type {
                    None => Err(Error::new_with_range(
                        ErrorType::TypeCheck,
                        format!(
                            "Cannot assign value to return type, expected '{}' but got '{}'",
                            self.current_function_return_type, expr_type,
                        ),
                        *range,
                    )),
                    Some(type_id) => {
                        if type_id != expr_type {
                            let widen_expression = Expression::Widen(WidenExpr {
                                expr_id,
                                range: *range,
                            });
                            expr_id = self.add_expression(widen_expression, type_id)?;
                        }

                        let statement = Statement::Return(ReturnStatement {
                            expr_id,
                            range: *range,
                        });

                        Ok(self.ast.add_statement(statement))
                    }
                }
            }
        }
    }

    fn check_expression(&mut self, expression_id: ParsedExpressionId) -> Result<ExpressionId> {
        let expression = self.parsed_ast.get_expression(expression_id);
        match expression {
            ParsedExpression::BinaryOperator {
                op_type,
                left,
                right,
                range,
            } => {
                // TODO: add support for logic operators
                let mut left_id = self.check_expression(*left)?;
                let left_type = self.get_type(left_id);
                let left_size = self.get_type_size(left_type)?;

                let mut right_id = self.check_expression(*right)?;
                let right_type = self.get_type(right_id);
                let right_size = self.get_type_size(right_type)?;

                // NOTE: We currently don't support this operation as for the eight bit variant,
                // the remainder gets stored in the ah register instead of dl. When we can output
                // assembly using the higher half eight bit registers, we can add this
                // functionality.
                if left_size == 8 && right_size == 8 && *op_type == BinaryOperatorType::Modulo {
                    return Err(Error::new_with_range(
                        ErrorType::TypeCheck,
                        "Modulo of two 8 bit integers is currently not supported".to_string(),
                        *range,
                    ));
                }

                // TODO: this needs a thorough rework, comparing references won't work
                if self.get_type_info(left_type)?.is_ptr() && !op_type.is_comparison() {
                    // TODO: limit this to only addition and subtraction
                    let right_id = self.add_expression(
                        Expression::Widen(WidenExpr {
                            expr_id: right_id,
                            range: *range,
                        }),
                        left_type,
                    )?;

                    let expression = Expression::BinaryOperator(BinaryOperatorExpr {
                        op_type: *op_type,
                        left_id,
                        right_id,
                        range: *range,
                    });
                    return self.add_expression(expression, left_type);
                }

                let result_type = match left_size.cmp(&right_size) {
                    Ordering::Greater => {
                        right_id = self.add_expression(
                            Expression::Widen(WidenExpr {
                                expr_id: right_id,
                                range: *range,
                            }),
                            left_type,
                        )?;
                        left_type
                    }
                    Ordering::Less => {
                        left_id = self.add_expression(
                            Expression::Widen(WidenExpr {
                                expr_id: left_id,
                                range: *range,
                            }),
                            right_type,
                        )?;
                        right_type
                    }
                    _ => left_type,
                };

                assert_eq!(self.get_type(left_id), self.get_type(right_id));

                let result_type = if op_type.is_comparison() {
                    BUILTIN_TYPE_BOOL
                } else {
                    result_type
                };

                let expression = Expression::BinaryOperator(BinaryOperatorExpr {
                    op_type: *op_type,
                    left_id,
                    right_id,
                    range: *range,
                });

                self.add_expression(expression, result_type)
            }
            ParsedExpression::UnaryOperator {
                op_type,
                expr,
                range,
            } => {
                let expr_id = self.check_expression(*expr)?;
                let expr_type = self.get_type(expr_id);

                let (expression, type_id) = match op_type {
                    UnaryOperatorType::Negate => (
                        Expression::UnaryOperator(UnaryOperatorExpr {
                            op_type: UnaryOperatorType::Negate,
                            expr_id,
                            range: *range,
                        }),
                        expr_type,
                    ),
                    UnaryOperatorType::Ref => (
                        Expression::UnaryOperator(UnaryOperatorExpr {
                            op_type: UnaryOperatorType::Ref,
                            expr_id,
                            range: *range,
                        }),
                        self.find_or_add_type(Type::Ptr(expr_type)),
                    ),
                    UnaryOperatorType::Deref => (
                        Expression::UnaryOperator(UnaryOperatorExpr {
                            op_type: UnaryOperatorType::Deref,
                            expr_id,
                            range: *range,
                        }),
                        self.get_type_info(expr_type)?.get_deref()?,
                    ),
                };

                self.add_expression(expression, type_id)
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
                                *range,
                            )
                        })?;

                let return_type = function_type.return_type;

                // NOTE: BUILTIN_TYPE_UNKNOWN is only used for extern functions
                if return_type == BUILTIN_TYPE_UNKNOWN {
                    let expression = Expression::FunctionCall(FunctionCallExpr {
                        name,
                        arg_ids: args
                            .iter()
                            .map(|arg| self.check_expression(*arg))
                            .collect::<Result<Vec<_>>>()?,
                        range: *range,
                    });
                    return self.add_expression(expression, BUILTIN_TYPE_U64);
                }

                assert_eq!(args.len(), function_type.parameters.len());

                let mut new_args = vec![];

                for (arg, expected_type) in args.iter().zip(function_type.parameters.iter()) {
                    let arg_id = self.check_expression(*arg)?;
                    let arg_type = self.get_type(arg_id);

                    let new_type = self
                        .widen_assignment(*expected_type, arg_type)?
                        .ok_or_else(|| {
                            Error::new_with_range(
                                ErrorType::TypeCheck,
                                format!("Cannot widen from type {arg_type:?} to {expected_type:?}"),
                                *range,
                            )
                        })?;

                    if new_type != arg_type {
                        let expression = Expression::Widen(WidenExpr {
                            expr_id: arg_id,
                            range: *range,
                        });
                        new_args.push(self.add_expression(expression, new_type)?);
                    } else {
                        new_args.push(arg_id);
                    }
                }

                let expression = Expression::FunctionCall(FunctionCallExpr {
                    name,
                    arg_ids: new_args,
                    range: *range,
                });
                self.add_expression(expression, return_type)
            }
            ParsedExpression::IntrinsicCall {
                name,
                arguments,
                range,
            } => match *name {
                "cast" => {
                    if arguments.len() != 2 {
                        return Err(Error::new_with_range(
                            ErrorType::TypeCheck,
                            "Intrinsic function 'cast' expects two arguments".to_string(),
                            *range,
                        ));
                    }

                    let checked_argument = self.check_expression(arguments[0])?;

                    let target_type_id = self.check_expression(arguments[1])?;
                    let target_type = self.ast.get_expression(target_type_id);

                    if let Expression::Type(_) = target_type {
                        let expression = Expression::Cast(CastExpr {
                            expr_id: checked_argument,
                            range: *range,
                        });
                        self.add_expression(expression, self.get_type(target_type_id))
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
                    *range,
                )),
            },
            ParsedExpression::IntegerLiteral { value, range } => {
                let expression = Expression::IntegerLiteral(IntegerLiteralExpr {
                    value: *value,
                    range: *range,
                });

                let type_id = match *value {
                    x if x < 2_u64.pow(8) => BUILTIN_TYPE_U8,
                    x if x < 2_u64.pow(16) => BUILTIN_TYPE_U16,
                    x if x < 2_u64.pow(32) => BUILTIN_TYPE_U32,
                    _ => BUILTIN_TYPE_U64,
                };

                self.add_expression(expression, type_id)
            }
            ParsedExpression::BooleanLiteral { value, range } => {
                let expression = Expression::BooleanLiteral(BooleanLiteralExpr {
                    value: *value,
                    range: *range,
                });
                self.add_expression(expression, BUILTIN_TYPE_BOOL)
            }
            ParsedExpression::VariableRef { name, range } => {
                let declaration_id = self.scope.find(name).map_err(|e| e.with_range(*range))?;
                let declaration = self.get_declaration(declaration_id);

                let expression = Expression::VariableRef(VariableRefExpr {
                    name,
                    declaration_id,
                    range: *range,
                });
                self.add_expression(expression, declaration.type_id)
            }
            ParsedExpression::StructLiteral {
                struct_type,
                fields,
                range,
            } => {
                let struct_type_id = self.check_type(struct_type)?;

                let struct_type = self
                    .get_struct(struct_type_id)
                    .map_err(|e| e.with_range(*range))?;

                for (expected_name, _) in &struct_type.fields {
                    if !fields.iter().any(|f| f.0 == expected_name) {
                        return Err(Error::new_with_range(
                            ErrorType::TypeCheck,
                            format!("Missing field '{expected_name}' in struct constructor"),
                            *range,
                        ));
                    }
                }

                if struct_type.fields.len() != fields.len() {
                    return Err(Error::new_with_range(
                        ErrorType::TypeCheck,
                        format!(
                            "Too many fields in struct constructor for '{}'",
                            struct_type.name
                        ),
                        *range,
                    ));
                }

                let mut checked_fields = vec![];

                for (field_name, field_value) in fields {
                    let mut checked_value_id = self.check_expression(*field_value)?;
                    let checked_value_type = self.get_type(checked_value_id);

                    // TODO: this refetch is stupid
                    let struct_type = self
                        .get_struct(struct_type_id)
                        .map_err(|e| e.with_range(*range))?;

                    let struct_field = struct_type
                        .fields
                        .iter()
                        .find(|x| x.0 == *field_name)
                        // NOTE: unwrap here is safe as struct fields are checked earlier
                        .unwrap();

                    let new_type = self
                        .widen_assignment(struct_field.1, checked_value_type)?
                        .ok_or_else(|| {
                            Error::new_with_range(
                                ErrorType::TypeCheck,
                                format!(
                                    "Cannot widen from type {} to {}",
                                    checked_value_type, struct_field.1,
                                ),
                                *range,
                            )
                        })?;

                    if new_type != checked_value_type {
                        checked_value_id = self.add_expression(
                            Expression::Widen(WidenExpr {
                                expr_id: checked_value_id,
                                range: *range,
                            }),
                            new_type,
                        )?;
                    }

                    checked_fields.push((*field_name, checked_value_id));
                }

                let expression = Expression::StructLiteral(StructLiteralExpr {
                    fields: checked_fields,
                    range: *range,
                });
                self.add_expression(expression, struct_type_id)
            }
            ParsedExpression::FieldAccessor { child, name, range } => {
                let child_id = self.check_expression(*child)?;
                let child_type_id = self.get_type(child_id);
                let child_type = self.get_type_info(child_type_id)?;

                if !child_type.is_struct() && !child_type.is_ptr() {
                    return Err(Error::new_with_range(
                        ErrorType::TypeCheck,
                        format!("Trying to access field '{name}' of non-struct type"),
                        *range,
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
                    Some(field_type) => {
                        let expression = Expression::FieldAccessor(FieldAccessorExpr {
                            name,
                            expr_id: child_id,
                            range: *range,
                        });
                        self.add_expression(expression, *field_type)
                    }
                    None => Err(Error::new_with_range(
                        ErrorType::TypeCheck,
                        format!("No field '{name}' exists on type {}", *child_type_id),
                        *range,
                    )),
                }
            }
            ParsedExpression::ArrayAccessor { expr, index, range } => {
                let expr_id = self.check_expression(*expr)?;
                let expr_type_id = self.get_type(expr_id);

                let expr_type = self.get_type_info(expr_type_id)?;

                if !expr_type.is_ptr() {
                    return Err(Error::new_with_range(
                        ErrorType::TypeCheck,
                        "Cannot user array accessor of non-pointer type".to_string(),
                        *range,
                    ));
                }

                let inner_type = expr_type.get_deref()?;

                // Convert $expr[$index] into *($expr + $index)
                let index = Expression::Widen(WidenExpr {
                    expr_id: self.check_expression(*index)?,
                    range: *range,
                });

                let index_range = *index.range();
                // TODO: on non-64-bit platforms this should be U32
                let index_expression = self.add_expression(index, BUILTIN_TYPE_U64)?;

                let add_expression = self.add_expression(
                    Expression::BinaryOperator(BinaryOperatorExpr {
                        op_type: BinaryOperatorType::Add,
                        left_id: expr_id,
                        right_id: index_expression,
                        range: index_range,
                    }),
                    BUILTIN_TYPE_U64,
                )?;

                let expression = Expression::UnaryOperator(UnaryOperatorExpr {
                    op_type: UnaryOperatorType::Deref,
                    expr_id: add_expression,
                    range: *range,
                });

                self.add_expression(expression, inner_type)
            }
            ParsedExpression::StringLiteral { value, range } => {
                let expression = Expression::StringLiteral(StringLiteralExpr {
                    value: *value,
                    range: *range,
                });
                let u8_ptr_type = self.find_or_add_type(Type::Ptr(BUILTIN_TYPE_U8));
                self.add_expression(expression, u8_ptr_type)
            }
            ParsedExpression::Type { value, range } => {
                let type_id = self.check_type(value)?;

                let expression = Expression::Type(TypeExpr { range: *range });
                self.add_expression(expression, type_id)
            }
        }
    }

    fn typecheck_upper_statement(
        &mut self,
        statement_id: ParsedUpperStatementId,
    ) -> Result<UpperStatementId> {
        let statement = self.parsed_ast.get_upper_statement(statement_id);
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
                            *range,
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
                    let declaration_id = self.add_declaration(*param_type, false);

                    self.scope
                        .insert(name, declaration_id)
                        .map_err(|x| x.with_range(*range))?;

                    params.push(declaration_id)
                }

                let checked_body = self.check_statement(*body)?;

                self.current_function_return_type = BUILTIN_TYPE_UNKNOWN;

                self.scope.pop();

                let checked_annotations = annotations
                    .iter()
                    .map(|(name, value)| {
                        if let Some(value) = value {
                            match self.check_expression(*value) {
                                Ok(value) => Ok((*name, Some(value))),
                                Err(error) => Err(error),
                            }
                        } else {
                            Ok((*name, None))
                        }
                    })
                    .collect::<Result<HashMap<_, _>>>()?;

                let statement = UpperStatement::Function(FunctionDeclaration {
                    name,
                    params,
                    return_type: function_declaration.return_type,
                    body: checked_body,
                    annotations: checked_annotations.into(),
                    range: *range,
                });
                Ok(self.ast.add_upper_statement(statement))
            }
            ParsedUpperStatement::StructDeclaration {
                name,
                fields,
                range,
            } => {
                let checked_fields = fields
                    .iter()
                    .map(|(name, parsed_type)| match self.check_type(parsed_type) {
                        Ok(checked_type) => Ok((*name, checked_type)),
                        Err(e) => Err(e),
                    })
                    .collect::<Result<Vec<_>>>()
                    .map_err(|x| x.with_range(*range))?;

                let complete_type = Type::Struct(StructType {
                    name: name.to_string(),
                    fields: checked_fields
                        .iter()
                        .map(|(x, y)| (x.to_string(), *y))
                        .collect::<Vec<_>>(),
                });
                let type_id = self.find_or_add_type(complete_type);

                let declaration_id = self.add_declaration(type_id, false);

                let statement = UpperStatement::StructDeclaration(StructDeclaration {
                    name,
                    declaration_id,
                    fields: checked_fields,
                    range: *range,
                });

                Ok(self.ast.add_upper_statement(statement))
            }
            ParsedUpperStatement::ConstDeclaration {
                name,
                value_type,
                value,
                annotations,
                range,
            } => {
                let type_id = self.check_type(value_type)?;

                let mut value_id = self.check_expression(*value)?;
                let value_type = self.get_type(value_id);

                let new_type = self.widen_assignment(type_id, value_type)?.ok_or_else(|| {
                    Error::new_with_range(
                        ErrorType::TypeCheck,
                        format!("Cannot widen const type {} to {}", value_type, type_id),
                        *range,
                    )
                })?;

                if new_type != value_type {
                    let expression = Expression::Widen(WidenExpr {
                        expr_id: value_id,
                        range: *range,
                    });
                    value_id = self.add_expression(expression, new_type)?;
                }

                let declaration_id = self.add_declaration(type_id, true);
                self.add_declaration_value(declaration_id, value_id);

                self.scope.insert(name, declaration_id)?;

                let checked_annotations = annotations
                    .iter()
                    .map(|(name, value)| {
                        if let Some(value) = value {
                            match self.check_expression(*value) {
                                Ok(value) => Ok((*name, Some(value))),
                                Err(error) => Err(error),
                            }
                        } else {
                            Ok((*name, None))
                        }
                    })
                    .collect::<Result<HashMap<_, _>>>()?;

                let statement = UpperStatement::ConstDeclaration(ConstDeclaration {
                    declaration_id,
                    value: value_id,
                    annotations: checked_annotations.into(),
                    range: *range,
                });
                Ok(self.ast.add_upper_statement(statement))
            }
            ParsedUpperStatement::ExternDeclaration { name, range } => {
                self.function_declarations.insert(
                    name,
                    FunctionType {
                        parameters: vec![],
                        return_type: BUILTIN_TYPE_UNKNOWN,
                    },
                );

                let statement = UpperStatement::ExternDeclaration(ExternDeclaration {
                    name,
                    range: *range,
                });
                Ok(self.ast.add_upper_statement(statement))
            }
        }
    }

    pub fn analyse(&mut self) -> Result<()> {
        self.register_upper_statements(&self.parsed_ast.upper_statements[..])?;

        for i in 0..self.parsed_ast.upper_statements.len() {
            self.typecheck_upper_statement((i as u32).into())?;
        }

        Ok(())
    }
}
