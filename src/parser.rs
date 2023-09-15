use std::collections::HashMap;

use crate::{
    ast::{BinaryOperatorType, EscapedString, UnaryOperatorType},
    error::{Error, ErrorType, Result},
    id_impl,
    lexer::{SourceRange, Token, TokenType},
};

#[derive(Debug, PartialEq)]
pub enum ParsedType {
    Named(String, SourceRange),
    Ptr(Box<ParsedType>, SourceRange),
}

pub struct ParsedAst<'a> {
    pub expressions: Vec<ParsedExpression<'a>>,
    pub statements: Vec<ParsedStatement<'a>>,
    pub upper_statements: Vec<ParsedUpperStatement<'a>>,
}

impl<'a> Default for ParsedAst<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> ParsedAst<'a> {
    pub fn new() -> Self {
        Self {
            expressions: vec![],
            statements: vec![],
            upper_statements: vec![],
        }
    }

    fn add_expression(&mut self, expression: ParsedExpression<'a>) -> ParsedExpressionId {
        self.expressions.push(expression);
        ParsedExpressionId(self.expressions.len() as u32 - 1)
    }

    fn add_statement(&mut self, statement: ParsedStatement<'a>) -> ParsedStatementId {
        self.statements.push(statement);
        ParsedStatementId(self.statements.len() as u32 - 1)
    }

    fn add_upper_statement(
        &mut self,
        upper_statement: ParsedUpperStatement<'a>,
    ) -> ParsedUpperStatementId {
        self.upper_statements.push(upper_statement);
        ParsedUpperStatementId(self.upper_statements.len() as u32 - 1)
    }

    pub fn get_expression(&self, expression_id: ParsedExpressionId) -> &'a ParsedExpression {
        &self.expressions[*expression_id as usize]
    }

    pub fn get_statement(&self, statement_id: ParsedStatementId) -> &ParsedStatement {
        &self.statements[*statement_id as usize]
    }

    pub fn get_upper_statement(
        &self,
        upper_statement_id: ParsedUpperStatementId,
    ) -> &ParsedUpperStatement {
        &self.upper_statements[*upper_statement_id as usize]
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct ParsedExpressionId(u32);
id_impl!(ParsedExpressionId);

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct ParsedStatementId(u32);
id_impl!(ParsedStatementId);

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct ParsedUpperStatementId(u32);
id_impl!(ParsedUpperStatementId);

pub type ParsedAnnotations<'a> = HashMap<&'a str, Option<ParsedExpressionId>>;
pub type NameTypePairs<'a> = Vec<(&'a str, ParsedType)>;

#[derive(Debug, PartialEq)]
pub enum ParsedUpperStatement<'a> {
    Function {
        name: &'a str,
        parameters: NameTypePairs<'a>,
        return_type: ParsedType,
        body: ParsedStatementId,
        annotations: ParsedAnnotations<'a>,
        range: SourceRange,
    },
    StructDeclaration {
        name: &'a str,
        fields: NameTypePairs<'a>,
        range: SourceRange,
    },
    ConstDeclaration {
        name: &'a str,
        value_type: ParsedType,
        value: ParsedExpressionId,
        annotations: ParsedAnnotations<'a>,
        range: SourceRange,
    },
    ExternDeclaration {
        name: &'a str,
        range: SourceRange,
    },
}

#[derive(Debug, PartialEq)]
pub enum ParsedStatement<'a> {
    Block {
        children: Vec<ParsedStatementId>,
        scoped: bool,
        range: SourceRange,
    },
    Declaration {
        name: &'a str,
        value_type: ParsedType,
        range: SourceRange,
    },
    Assignment {
        left: ParsedExpressionId,
        right: ParsedExpressionId,
        range: SourceRange,
    },
    Expression {
        expr: ParsedExpressionId,
        range: SourceRange,
    },
    While {
        condition: ParsedExpressionId,
        body: ParsedStatementId,
        range: SourceRange,
    },
    If {
        condition: ParsedExpressionId,
        body: ParsedStatementId,
        else_body: Option<ParsedStatementId>,
        range: SourceRange,
    },
    Return {
        value: ParsedExpressionId,
        range: SourceRange,
    },
}

#[derive(Debug, PartialEq)]
pub enum ParsedExpression<'a> {
    BinaryOperator {
        op_type: BinaryOperatorType,
        left: ParsedExpressionId,
        right: ParsedExpressionId,
        range: SourceRange,
    },
    UnaryOperator {
        op_type: UnaryOperatorType,
        expr: ParsedExpressionId,
        range: SourceRange,
    },
    FunctionCall {
        name: &'a str,
        arguments: Vec<ParsedExpressionId>,
        range: SourceRange,
    },
    IntrinsicCall {
        name: &'a str,
        arguments: Vec<ParsedExpressionId>,
        range: SourceRange,
    },
    IntegerLiteral {
        value: u64,
        range: SourceRange,
    },
    BooleanLiteral {
        value: bool,
        range: SourceRange,
    },
    StructLiteral {
        struct_type: ParsedType,
        fields: Vec<(&'a str, ParsedExpressionId)>,
        range: SourceRange,
    },
    VariableRef {
        name: &'a str,
        range: SourceRange,
    },
    FieldAccessor {
        name: &'a str,
        child: ParsedExpressionId,
        range: SourceRange,
    },
    ArrayAccessor {
        expr: ParsedExpressionId,
        index: ParsedExpressionId,
        range: SourceRange,
    },
    StringLiteral {
        value: EscapedString<'a>,
        range: SourceRange,
    },
    Type {
        value: ParsedType,
        range: SourceRange,
    },
}

type PrefixParseFn<'a> = fn(&mut Parser<'a>) -> Result<ParsedExpressionId>;

type InfixParseFn<'a> = fn(
    &mut Parser<'a>,
    start_index: usize,
    left: ParsedExpressionId,
    precedence: usize,
) -> Result<ParsedExpressionId>;

pub struct Parser<'a> {
    tokens: &'a [Token<'a>],
    index: usize,
    prefix_fns: HashMap<TokenType, PrefixParseFn<'a>>,
    infix_fns: HashMap<TokenType, (InfixParseFn<'a>, usize)>,
    ast: ParsedAst<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        let mut prefix_fns: HashMap<_, PrefixParseFn<'a>> = HashMap::new();
        prefix_fns.insert(
            TokenType::Identifier,
            Self::parse_identifier_or_function_call,
        );
        prefix_fns.insert(TokenType::At, Self::parse_intrinsic_call);
        prefix_fns.insert(TokenType::Minus, Self::parse_unary_expression);
        prefix_fns.insert(TokenType::Ampersand, Self::parse_unary_expression);
        prefix_fns.insert(TokenType::Asterisk, Self::parse_unary_expression);
        prefix_fns.insert(TokenType::True, Self::parse_constant);
        prefix_fns.insert(TokenType::False, Self::parse_constant);
        prefix_fns.insert(TokenType::IntegerLiteral, Self::parse_constant);
        prefix_fns.insert(TokenType::CharLiteral, Self::parse_constant);
        prefix_fns.insert(TokenType::BinaryIntegerLiteral, Self::parse_constant);
        prefix_fns.insert(TokenType::OctalIntegerLiteral, Self::parse_constant);
        prefix_fns.insert(TokenType::HexIntegerLiteral, Self::parse_constant);
        prefix_fns.insert(TokenType::StringLiteral, Self::parse_constant);
        prefix_fns.insert(TokenType::Struct, Self::parse_struct_constant);
        prefix_fns.insert(TokenType::LeftParen, Self::parse_parenthesized);
        prefix_fns.insert(TokenType::Colon, Self::parse_type_value);

        let mut infix_fns: HashMap<_, (InfixParseFn<'a>, usize)> = HashMap::new();
        infix_fns.insert(TokenType::Plus, (Self::parse_binary_operator, 11));
        infix_fns.insert(TokenType::Minus, (Self::parse_binary_operator, 11));
        infix_fns.insert(TokenType::Asterisk, (Self::parse_binary_operator, 12));
        infix_fns.insert(TokenType::Slash, (Self::parse_binary_operator, 12));
        infix_fns.insert(TokenType::DoubleLessThan, (Self::parse_binary_operator, 10));
        infix_fns.insert(
            TokenType::DoubleGreaterThan,
            (Self::parse_binary_operator, 10),
        );
        infix_fns.insert(TokenType::Percent, (Self::parse_binary_operator, 12));
        infix_fns.insert(TokenType::DoubleEqual, (Self::parse_binary_operator, 6));
        infix_fns.insert(TokenType::NotEqual, (Self::parse_binary_operator, 6));
        infix_fns.insert(TokenType::LessThan, (Self::parse_binary_operator, 6));
        infix_fns.insert(TokenType::LessThanEqual, (Self::parse_binary_operator, 6));
        infix_fns.insert(TokenType::GreaterThan, (Self::parse_binary_operator, 6));
        infix_fns.insert(
            TokenType::GreaterThanEqual,
            (Self::parse_binary_operator, 6),
        );
        infix_fns.insert(
            TokenType::DoubleVerticalBar,
            (Self::parse_binary_operator, 6),
        );
        infix_fns.insert(TokenType::DoubleAmpersand, (Self::parse_binary_operator, 6));

        infix_fns.insert(TokenType::Dot, (Self::parse_field_accessor, 13));
        infix_fns.insert(TokenType::LeftSquareParen, (Self::parse_array_accessor, 13));

        Self {
            tokens,
            index: 0,
            prefix_fns,
            infix_fns,
            ast: ParsedAst::new(),
        }
    }

    pub fn parse(mut self) -> Result<ParsedAst<'a>> {
        while !self.eof() {
            self.parse_upper_statement()?;
        }
        Ok(self.ast)
    }

    fn current_index(&self, end: bool) -> usize {
        if self.eof() {
            self.tokens.last().map(|t| t.range.end).unwrap_or(0)
        } else if end {
            self.tokens[self.index - 1].range.end
        } else {
            self.tokens[self.index].range.start
        }
    }

    fn eof(&self) -> bool {
        self.index >= self.tokens.len()
    }

    fn peek(&self) -> Result<&'a Token<'a>> {
        if self.eof() {
            Err(Error::new_with_range(
                ErrorType::Parser,
                "Token stream out of bounds".to_string(),
                self.tokens
                    .last()
                    .map(|t| t.range)
                    .unwrap_or(SourceRange::empty()),
            ))
        } else {
            Ok(&self.tokens[self.index])
        }
    }

    fn peeks(&self, i: usize) -> Result<&'a Token<'a>> {
        if self.index + i >= self.tokens.len() {
            Err(Error::new_with_range(
                ErrorType::Parser,
                "Token stream out of bounds".to_string(),
                self.tokens
                    .last()
                    .map(|t| t.range)
                    .unwrap_or(SourceRange::empty()),
            ))
        } else {
            Ok(&self.tokens[self.index + i])
        }
    }

    fn consume(&mut self) -> Result<&'a Token<'a>> {
        let token = self.peek()?;
        self.index += 1;
        Ok(token)
    }

    fn consume_assert(&mut self, token_type: TokenType) -> Result<&'a Token<'a>> {
        let token = self.consume()?;
        if token.token_type != token_type {
            return Err(Error::new_with_range(
                ErrorType::Parser,
                format!(
                    "Expected token {:?}, but got {:?}",
                    token_type, token.token_type
                ),
                token.range,
            ));
        }
        Ok(token)
    }

    fn parse_type(&mut self) -> Result<ParsedType> {
        let token = self.consume_assert(TokenType::Identifier)?;
        let mut result = ParsedType::Named(token.value.to_string(), token.range);

        if self.peek()?.token_type == TokenType::Asterisk {
            self.consume()?;

            result = ParsedType::Ptr(Box::new(result), self.peek()?.range.expand(&token.range));
        }

        Ok(result)
    }

    fn parse_function_call(&mut self) -> Result<ParsedExpressionId> {
        let function_start = self.current_index(false);
        let name = self.consume_assert(TokenType::Identifier)?.value;
        self.consume_assert(TokenType::LeftParen)?;

        let mut arguments = vec![];
        loop {
            if self.peek()?.token_type == TokenType::RightParen {
                break;
            }

            arguments.push(self.parse_expression(0)?);

            if self.peek()?.token_type == TokenType::RightParen {
                break;
            }

            self.consume_assert(TokenType::Comma)?;
        }

        self.consume_assert(TokenType::RightParen)?;

        let expression = ParsedExpression::FunctionCall {
            name,
            arguments,
            range: (function_start..self.current_index(true)).into(),
        };

        Ok(self.ast.add_expression(expression))
    }

    fn parse_intrinsic_call(&mut self) -> Result<ParsedExpressionId> {
        let function_start = self.current_index(false);

        self.consume_assert(TokenType::At)?;
        let name = self.consume_assert(TokenType::Identifier)?.value;
        self.consume_assert(TokenType::LeftParen)?;

        let mut arguments = vec![];
        loop {
            if self.peek()?.token_type == TokenType::RightParen {
                break;
            }

            arguments.push(self.parse_expression(0)?);

            if self.peek()?.token_type == TokenType::RightParen {
                break;
            }

            self.consume_assert(TokenType::Comma)?;
        }

        self.consume_assert(TokenType::RightParen)?;
        let expression = ParsedExpression::IntrinsicCall {
            name,
            arguments,
            range: (function_start..self.current_index(true)).into(),
        };

        Ok(self.ast.add_expression(expression))
    }

    fn parse_field_accessor(
        &mut self,
        start_index: usize,
        left: ParsedExpressionId,
        _precedence: usize,
    ) -> Result<ParsedExpressionId> {
        self.consume_assert(TokenType::Dot)?;
        let name = self.consume_assert(TokenType::Identifier)?.value;

        let expression = ParsedExpression::FieldAccessor {
            name,
            child: left,
            range: (start_index..self.current_index(true)).into(),
        };
        Ok(self.ast.add_expression(expression))
    }

    fn parse_array_accessor(
        &mut self,
        start_index: usize,
        left: ParsedExpressionId,
        _precedence: usize,
    ) -> Result<ParsedExpressionId> {
        self.consume_assert(TokenType::LeftSquareParen)?;
        let index = self.parse_expression(0)?;
        self.consume_assert(TokenType::RightSquareParen)?;

        let expression = ParsedExpression::ArrayAccessor {
            expr: left,
            index,
            range: (start_index..self.current_index(true)).into(),
        };

        Ok(self.ast.add_expression(expression))
    }

    fn parse_identifier_or_function_call(&mut self) -> Result<ParsedExpressionId> {
        let identifier_start = self.current_index(false);
        let next_token = self.peeks(1)?.token_type;

        match next_token {
            TokenType::LeftParen => self.parse_function_call(),
            _ => {
                let name = self.consume_assert(TokenType::Identifier)?.value;

                let expression = ParsedExpression::VariableRef {
                    name,
                    range: (identifier_start..self.current_index(true)).into(),
                };
                Ok(self.ast.add_expression(expression))
            }
        }
    }

    fn parse_unary_expression(&mut self) -> Result<ParsedExpressionId> {
        let unary_start = self.current_index(false);
        let token = self.consume()?;
        let op_type = UnaryOperatorType::from_token_type(token.token_type);
        let expr = self.parse_expression(0)?;

        let unary_expression = ParsedExpression::UnaryOperator {
            op_type,
            expr,
            range: (unary_start..self.current_index(true)).into(),
        };

        Ok(self.ast.add_expression(unary_expression))
    }

    fn parse_integer_literal(
        &mut self,
        value: &str,
        prefix: &str,
        radix: u32,
        range: SourceRange,
    ) -> Result<ParsedExpressionId> {
        let Ok(value) = u64::from_str_radix(
            value
                .trim_start_matches(prefix)
                .trim_start_matches(&prefix.to_uppercase()),
            radix,
        ) else {
            return Err(Error::new_with_range(
                ErrorType::Parser,
                format!("Failed to parse '{value}' to int"),
                range,
            ));
        };

        Ok(self
            .ast
            .add_expression(ParsedExpression::IntegerLiteral { value, range }))
    }

    fn parse_constant(&mut self) -> Result<ParsedExpressionId> {
        let constant_start = self.current_index(false);
        let token = self.consume()?;
        let range = (constant_start..self.current_index(true)).into();
        match token.token_type {
            TokenType::True => Ok(self
                .ast
                .add_expression(ParsedExpression::BooleanLiteral { value: true, range })),
            TokenType::False => Ok(self.ast.add_expression(ParsedExpression::BooleanLiteral {
                value: false,
                range,
            })),
            TokenType::IntegerLiteral => self.parse_integer_literal(token.value, "", 10, range),
            TokenType::CharLiteral => {
                Ok(self.ast.add_expression(ParsedExpression::IntegerLiteral {
                    // TODO: character escaping
                    value: token.value.as_bytes()[1] as u64,
                    range,
                }))
            }
            TokenType::BinaryIntegerLiteral => {
                self.parse_integer_literal(token.value, "0b", 2, range)
            }
            TokenType::OctalIntegerLiteral => {
                self.parse_integer_literal(token.value, "0o", 8, range)
            }
            TokenType::HexIntegerLiteral => {
                self.parse_integer_literal(token.value, "0x", 16, range)
            }
            TokenType::StringLiteral => {
                Ok(self.ast.add_expression(ParsedExpression::StringLiteral {
                    value: token.value[1..token.value.len() - 1].into(),
                    range,
                }))
            }
            _ => Err(Error::new_with_range(
                ErrorType::Parser,
                format!("Parsing constant but got token {:?}", token.token_type),
                token.range,
            )),
        }
    }

    fn parse_type_value(&mut self) -> Result<ParsedExpressionId> {
        let start_index = self.current_index(false);

        // NOTE: colon before a type is required to not get an ambiguous syntax. Otherwise the
        // parser will not see the difference between 'u16' and 'foo' which might be a variable
        // reference and not a type.
        self.consume_assert(TokenType::Colon)?;

        let value = self.parse_type()?;

        let expression = ParsedExpression::Type {
            value,
            range: (start_index..self.current_index(true)).into(),
        };

        Ok(self.ast.add_expression(expression))
    }

    fn parse_struct_constant(&mut self) -> Result<ParsedExpressionId> {
        let start_index = self.current_index(false);

        self.consume_assert(TokenType::Struct)?;

        let struct_type = self.parse_type()?;

        self.consume_assert(TokenType::LeftBrace)?;

        let mut fields = vec![];

        while self.peek()?.token_type != TokenType::RightBrace {
            let name = self.consume_assert(TokenType::Identifier)?.value;

            self.consume_assert(TokenType::Equals)?;

            let value = self.parse_expression(0)?;

            self.consume_assert(TokenType::Comma)?;

            fields.push((name, value));
        }

        self.consume_assert(TokenType::RightBrace)?;

        let expression = ParsedExpression::StructLiteral {
            struct_type,
            fields,
            range: (start_index..self.current_index(true)).into(),
        };
        Ok(self.ast.add_expression(expression))
    }

    fn parse_parenthesized(&mut self) -> Result<ParsedExpressionId> {
        self.consume_assert(TokenType::LeftParen)?;
        let expr = self.parse_expression(0)?;
        self.consume_assert(TokenType::RightParen)?;
        Ok(expr)
    }

    fn parse_binary_operator(
        &mut self,
        start_index: usize,
        left: ParsedExpressionId,
        precedence: usize,
    ) -> Result<ParsedExpressionId> {
        let operator = self.consume()?;

        let op_type = BinaryOperatorType::from_token_type(operator.token_type);
        let right = self.parse_expression(precedence)?;
        let expression = ParsedExpression::BinaryOperator {
            op_type,
            left,
            right,
            range: (start_index..self.current_index(true)).into(),
        };

        Ok(self.ast.add_expression(expression))
    }

    fn parse_expression(&mut self, precedence: usize) -> Result<ParsedExpressionId> {
        let exit_tokens = [
            TokenType::SemiColon,
            TokenType::RightParen,
            TokenType::LeftBrace,
            TokenType::Comma,
            TokenType::Equals,
            TokenType::RightSquareParen,
        ];

        let token_type = self.peek()?.token_type;
        let start_index = self.current_index(false);

        let mut left = match self.prefix_fns.get(&token_type) {
            Some(func) => func(self),
            None => Err(Error::new_with_range(
                ErrorType::Parser,
                format!("Did not expect token '{token_type:?}' while parsing prefix expression"),
                self.peek()?.range,
            )),
        }?;

        if self.eof() {
            return Ok(left);
        }

        if exit_tokens.contains(&self.peek()?.token_type) {
            return Ok(left);
        }

        while !self.eof() {
            let token_type = self.peek()?.token_type;

            if exit_tokens.contains(&token_type) {
                break;
            }

            left = match self.infix_fns.get(&token_type) {
                Some((func, prec)) => {
                    let prec = *prec;
                    if precedence >= prec {
                        break;
                    }
                    func(self, start_index, left, prec)
                }
                None => Err(Error::new_with_range(
                    ErrorType::Parser,
                    format!("Did not expect token '{token_type:?}' while parsing infix expression"),
                    self.peek()?.range,
                )),
            }?;
        }

        Ok(left)
    }

    fn parse_return_statement(&mut self) -> Result<ParsedStatementId> {
        let return_start = self.current_index(false);
        self.consume_assert(TokenType::Return)?;

        let value = self.parse_expression(0)?;
        self.consume_assert(TokenType::SemiColon)?;

        let statement = ParsedStatement::Return {
            value,
            range: (return_start..self.current_index(true)).into(),
        };
        Ok(self.ast.add_statement(statement))
    }

    fn parse_let_statement(&mut self) -> Result<ParsedStatementId> {
        let let_start = self.current_index(false);
        self.consume_assert(TokenType::Let)?;
        let (name, value_type) = self.parse_identifier_type()?;

        if self.peek()?.token_type == TokenType::Equals {
            // We have a declaration and assignment combined
            self.consume_assert(TokenType::Equals)?;
            let decl_start = self.current_index(false);
            let expr = self.parse_expression(0)?;
            self.consume_assert(TokenType::SemiColon)?;

            let variable_ref_expr = self.ast.add_expression(ParsedExpression::VariableRef {
                name,
                range: (decl_start..self.current_index(true)).into(),
            });

            let children = vec![
                ParsedStatement::Declaration {
                    name,
                    value_type,
                    range: (let_start..decl_start).into(),
                },
                ParsedStatement::Assignment {
                    left: variable_ref_expr,
                    right: expr,
                    range: (decl_start..self.current_index(true)).into(),
                },
            ]
            .into_iter()
            .map(|s| self.ast.add_statement(s))
            .collect();

            let statement = ParsedStatement::Block {
                children,
                scoped: false,
                range: (let_start..self.current_index(true)).into(),
            };
            return Ok(self.ast.add_statement(statement));
        }

        self.consume_assert(TokenType::SemiColon)?;

        let statement = ParsedStatement::Declaration {
            name,
            value_type,
            range: (let_start..self.current_index(true)).into(),
        };
        Ok(self.ast.add_statement(statement))
    }

    fn parse_assignment_statement(&mut self) -> Result<ParsedStatementId> {
        let assignment_start = self.current_index(false);
        let left = self.parse_expression(0)?;
        self.consume_assert(TokenType::Equals)?;

        let right = self.parse_expression(0)?;
        self.consume_assert(TokenType::SemiColon)?;

        let statement = ParsedStatement::Assignment {
            left,
            right,
            range: (assignment_start..self.current_index(true)).into(),
        };
        Ok(self.ast.add_statement(statement))
    }

    fn parse_while_statement(&mut self) -> Result<ParsedStatementId> {
        let while_start = self.current_index(false);
        self.consume_assert(TokenType::While)?;

        let condition = self.parse_expression(0)?;
        let body = self.parse_statement()?;

        let statement = ParsedStatement::While {
            condition,
            body,
            range: (while_start..self.current_index(true)).into(),
        };
        Ok(self.ast.add_statement(statement))
    }

    fn parse_if_statement(&mut self) -> Result<ParsedStatementId> {
        let if_start = self.current_index(false);
        self.consume_assert(TokenType::If)?;

        let condition = self.parse_expression(0)?;
        let body = self.parse_statement()?;

        let else_body = if self.peek()?.token_type == TokenType::Else {
            self.consume_assert(TokenType::Else)?;
            Some(self.parse_statement()?)
        } else {
            None
        };

        let statement = ParsedStatement::If {
            condition,
            body,
            else_body,
            range: (if_start..self.current_index(true)).into(),
        };
        Ok(self.ast.add_statement(statement))
    }

    fn parse_identifier_type(&mut self) -> Result<(&'a str, ParsedType)> {
        let identifier = self.consume_assert(TokenType::Identifier)?.value;
        self.consume_assert(TokenType::Colon)?;

        let value_type = self.parse_type()?;

        Ok((identifier, value_type))
    }

    fn parse_function(
        &mut self,
        annotations: ParsedAnnotations<'a>,
    ) -> Result<ParsedUpperStatementId> {
        let function_start = self.current_index(false);
        self.consume_assert(TokenType::Fn)?;

        let name = self.consume_assert(TokenType::Identifier)?.value;
        self.consume_assert(TokenType::LeftParen)?;

        let mut parameters = vec![];

        loop {
            if self.peek()?.token_type == TokenType::RightParen {
                break;
            }

            let (name, value_type) = self.parse_identifier_type()?;
            parameters.push((name, value_type));

            if self.peek()?.token_type == TokenType::RightParen {
                break;
            }

            self.consume_assert(TokenType::Comma)?;
        }

        self.consume_assert(TokenType::RightParen)?;

        let return_type = if self.peek()?.token_type == TokenType::LeftBrace {
            // TODO: this range is not completely correct
            ParsedType::Named("void".to_string(), self.peek()?.range)
        } else {
            self.parse_type()?
        };

        let body = self.parse_statement()?;
        let statement = ParsedUpperStatement::Function {
            name,
            parameters,
            return_type,
            body,
            annotations,
            range: (function_start..self.current_index(true)).into(),
        };
        Ok(self.ast.add_upper_statement(statement))
    }

    fn parse_block(&mut self) -> Result<ParsedStatementId> {
        let block_start = self.current_index(false);
        self.consume_assert(TokenType::LeftBrace)?;
        let mut children = vec![];
        while self.peek()?.token_type != TokenType::RightBrace {
            children.push(self.parse_statement()?);
        }

        self.consume_assert(TokenType::RightBrace)?;

        let statement = ParsedStatement::Block {
            children,
            scoped: true,
            range: (block_start..self.current_index(true)).into(),
        };

        Ok(self.ast.add_statement(statement))
    }

    fn parse_statement(&mut self) -> Result<ParsedStatementId> {
        let token = self.peek()?;

        match token.token_type {
            TokenType::Return => self.parse_return_statement(),
            TokenType::LeftBrace => self.parse_block(),
            TokenType::Let => self.parse_let_statement(),
            TokenType::While => self.parse_while_statement(),
            TokenType::If => self.parse_if_statement(),
            TokenType::Identifier => match self.peeks(1)?.token_type {
                TokenType::LeftParen => {
                    let expr_start = self.current_index(false);
                    let statement = ParsedStatement::Expression {
                        expr: self.parse_expression(0)?,
                        range: (expr_start..self.current_index(true) + 1).into(),
                    };
                    self.consume_assert(TokenType::SemiColon)?;

                    Ok(self.ast.add_statement(statement))
                }
                _ => self.parse_assignment_statement(),
            },
            _ => self.parse_assignment_statement(),
        }
    }

    fn parse_const_declaration(
        &mut self,
        annotations: ParsedAnnotations<'a>,
    ) -> Result<ParsedUpperStatementId> {
        let start_index = self.current_index(false);

        self.consume_assert(TokenType::Const)?;
        let name = self.consume_assert(TokenType::Identifier)?.value;

        self.consume_assert(TokenType::Colon)?;
        let value_type = self.parse_type()?;

        self.consume_assert(TokenType::Equals)?;
        let value = self.parse_expression(0)?;

        self.consume_assert(TokenType::SemiColon)?;

        let statement = ParsedUpperStatement::ConstDeclaration {
            name,
            value_type,
            value,
            annotations,
            range: (start_index..self.current_index(true)).into(),
        };
        Ok(self.ast.add_upper_statement(statement))
    }

    fn parse_extern(&mut self) -> Result<ParsedUpperStatementId> {
        let start_index = self.current_index(false);

        self.consume_assert(TokenType::Extern)?;
        let symbol = self.consume_assert(TokenType::Identifier)?.value;
        self.consume_assert(TokenType::SemiColon)?;

        let statement = ParsedUpperStatement::ExternDeclaration {
            name: symbol,
            range: (start_index..self.current_index(true)).into(),
        };
        Ok(self.ast.add_upper_statement(statement))
    }

    fn parse_struct_declaration(&mut self) -> Result<ParsedUpperStatementId> {
        let start_index = self.current_index(false);

        self.consume_assert(TokenType::Struct)?;
        let name = self.consume_assert(TokenType::Identifier)?.value;

        self.consume_assert(TokenType::LeftBrace)?;

        let mut fields = vec![];

        while self.peek()?.token_type != TokenType::RightBrace {
            let field_name = self.consume_assert(TokenType::Identifier)?.value;

            self.consume_assert(TokenType::Colon)?;

            let field_type = self.parse_type()?;

            self.consume_assert(TokenType::Comma)?;

            fields.push((field_name, field_type));
        }

        self.consume_assert(TokenType::RightBrace)?;

        let statement = ParsedUpperStatement::StructDeclaration {
            name,
            fields,
            range: (start_index..self.current_index(true)).into(),
        };
        Ok(self.ast.add_upper_statement(statement))
    }

    fn parse_annotations(&mut self) -> Result<ParsedAnnotations<'a>> {
        let mut annotations = HashMap::new();

        while self.peek()?.token_type == TokenType::At {
            self.consume_assert(TokenType::At)?;
            let name = self.consume_assert(TokenType::Identifier)?.value;

            if self.peek()?.token_type != TokenType::LeftParen {
                annotations.insert(name, None);
                continue;
            }

            self.consume_assert(TokenType::LeftParen)?;
            let value = self.parse_expression(0)?;
            self.consume_assert(TokenType::RightParen)?;

            annotations.insert(name, Some(value));
        }

        Ok(annotations)
    }

    fn parse_upper_statement(&mut self) -> Result<ParsedUpperStatementId> {
        let annotations = self.parse_annotations()?;

        let token = self.peek()?;

        match token.token_type {
            TokenType::Extern => self.parse_extern(),
            TokenType::Struct => self.parse_struct_declaration(),
            TokenType::Fn => self.parse_function(annotations),
            TokenType::Const => self.parse_const_declaration(annotations),
            _ => Err(Error::new_with_range(
                ErrorType::Parser,
                format!(
                    "Unexpected token {:?} for upper-level statement",
                    token.token_type
                ),
                token.range,
            )),
        }
    }
}
