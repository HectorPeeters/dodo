use std::collections::HashMap;

use crate::{
    ast::{BinaryOperatorType, UnaryOperatorType},
    error::{Error, ErrorType, Result},
    tokenizer::{SourceRange, Token, TokenType},
};

#[derive(Debug, PartialEq)]
pub enum ParsedType {
    Named(String),
    Ptr(Box<ParsedType>),
}

pub type ParsedAnnotations = Vec<(String, Option<ParsedExpression>)>;
pub type NameTypePairs = Vec<(String, ParsedType)>;

#[derive(Debug, PartialEq)]
pub enum ParsedUpperStatement {
    Function {
        name: String,
        parameters: NameTypePairs,
        return_type: ParsedType,
        body: ParsedStatement,
        annotations: ParsedAnnotations,
        range: SourceRange,
    },
    StructDeclaration {
        name: String,
        fields: NameTypePairs,
    },
    ConstDeclaration {
        name: String,
        value_type: ParsedType,
        value: ParsedExpression,
        range: SourceRange,
    },
    ExternDeclaration {
        name: String,
        range: SourceRange,
    },
}

#[derive(Debug, PartialEq)]
pub enum ParsedStatement {
    Block {
        children: Vec<ParsedStatement>,
        scoped: bool,
        range: SourceRange,
    },
    Declaration {
        name: String,
        value_type: ParsedType,
        range: SourceRange,
    },
    Assignment {
        left: ParsedExpression,
        right: ParsedExpression,
        range: SourceRange,
    },
    Expression {
        expr: ParsedExpression,
        range: SourceRange,
    },
    While {
        condition: ParsedExpression,
        body: Box<ParsedStatement>,
        range: SourceRange,
    },
    If {
        condition: ParsedExpression,
        body: Box<ParsedStatement>,
        range: SourceRange,
    },
    Return {
        value: ParsedExpression,
        range: SourceRange,
    },
}

#[derive(Debug, PartialEq)]
pub enum ParsedExpression {
    BinaryOperator {
        op_type: BinaryOperatorType,
        left: Box<ParsedExpression>,
        right: Box<ParsedExpression>,
        range: SourceRange,
    },
    UnaryOperator {
        op_type: UnaryOperatorType,
        expr: Box<ParsedExpression>,
        range: SourceRange,
    },
    FunctionCall {
        name: String,
        arguments: Vec<ParsedExpression>,
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
    VariableRef {
        name: String,
        range: SourceRange,
    },
    FieldAccessor {
        name: String,
        child: Box<ParsedExpression>,
        range: SourceRange,
    },
    StringLiteral {
        value: String,
        range: SourceRange,
    },
}

type PrefixParseFn<'a> = fn(&mut Parser<'a>) -> Result<ParsedExpression>;

type InfixParseFn<'a> = fn(
    &mut Parser<'a>,
    start_index: usize,
    left: ParsedExpression,
    precedence: usize,
) -> Result<ParsedExpression>;

type PostfixParseFn<'a> = fn(&mut Parser<'a>, left: ParsedExpression) -> Result<ParsedExpression>;

pub struct Parser<'a> {
    tokens: &'a [Token<'a>],
    index: usize,
    prefix_fns: HashMap<TokenType, PrefixParseFn<'a>>,
    infix_fns: HashMap<TokenType, (InfixParseFn<'a>, usize)>,
    postfix_fns: HashMap<TokenType, PostfixParseFn<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        let mut prefix_fns: HashMap<_, PrefixParseFn<'a>> = HashMap::new();
        prefix_fns.insert(
            TokenType::Identifier,
            Self::parse_identifier_or_function_call,
        );
        prefix_fns.insert(TokenType::Minus, Self::parse_unary_expression);
        prefix_fns.insert(TokenType::Ampersand, Self::parse_unary_expression);
        prefix_fns.insert(TokenType::Asterisk, Self::parse_unary_expression);
        prefix_fns.insert(TokenType::True, Self::parse_constant);
        prefix_fns.insert(TokenType::False, Self::parse_constant);
        prefix_fns.insert(TokenType::IntegerLiteral, Self::parse_constant);
        prefix_fns.insert(TokenType::BinaryIntegerLiteral, Self::parse_constant);
        prefix_fns.insert(TokenType::OctalIntegerLiteral, Self::parse_constant);
        prefix_fns.insert(TokenType::HexIntegerLiteral, Self::parse_constant);
        prefix_fns.insert(TokenType::StringLiteral, Self::parse_constant);
        prefix_fns.insert(TokenType::LeftParen, Self::parse_parenthesized);

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

        let mut postfix_fns: HashMap<_, PostfixParseFn<'a>> = HashMap::new();
        postfix_fns.insert(TokenType::Dot, Self::parse_field_accessor);

        Self {
            tokens,
            index: 0,
            prefix_fns,
            infix_fns,
            postfix_fns,
        }
    }

    fn current_index(&self, end: bool) -> usize {
        if self.eof() {
            return self.tokens.last().unwrap().range.end;
        }
        if end {
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
                self.tokens.last().unwrap().range,
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
                self.tokens.last().unwrap().range,
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
        let mut result = ParsedType::Named(
            self.consume_assert(TokenType::Identifier)?
                .value
                .to_string(),
        );

        if self.peek()?.token_type == TokenType::Asterisk {
            self.consume()?;

            result = ParsedType::Ptr(Box::new(result));
        }

        Ok(result)
    }

    fn parse_function_call(&mut self) -> Result<ParsedExpression> {
        let function_start = self.current_index(false);
        let name = self
            .consume_assert(TokenType::Identifier)?
            .value
            .to_string();
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
        Ok(ParsedExpression::FunctionCall {
            name,
            arguments,
            range: (function_start..self.current_index(true)).into(),
        })
    }

    fn parse_field_accessor(&mut self, left: ParsedExpression) -> Result<ParsedExpression> {
        let start_index = self.current_index(false);

        self.consume_assert(TokenType::Dot)?;
        let name = self
            .consume_assert(TokenType::Identifier)?
            .value
            .to_string();

        Ok(ParsedExpression::FieldAccessor {
            name,
            child: Box::new(left),
            range: (start_index..self.current_index(true)).into(),
        })
    }

    fn parse_identifier_or_function_call(&mut self) -> Result<ParsedExpression> {
        let identifier_start = self.current_index(false);
        let next_token = self.peeks(1)?.token_type;

        match next_token {
            TokenType::LeftParen => self.parse_function_call(),
            _ => {
                let name = self
                    .consume_assert(TokenType::Identifier)?
                    .value
                    .to_string();

                Ok(ParsedExpression::VariableRef {
                    name,
                    range: (identifier_start..self.current_index(true)).into(),
                })
            }
        }
    }

    fn parse_unary_expression(&mut self) -> Result<ParsedExpression> {
        let unary_start = self.current_index(false);
        let token = self.consume()?;
        let op_type = UnaryOperatorType::from_token_type(token.token_type);
        let expression = self.parse_expression(0)?;

        Ok(ParsedExpression::UnaryOperator {
            op_type,
            expr: Box::new(expression),
            range: (unary_start..self.current_index(true)).into(),
        })
    }

    fn parse_integer_literal(
        value: &str,
        prefix: &str,
        radix: u32,
        range: SourceRange,
    ) -> Result<ParsedExpression> {
        match u64::from_str_radix(
            value
                .trim_start_matches(prefix)
                .trim_start_matches(&prefix.to_uppercase()),
            radix,
        ) {
            Ok(value) => Ok(ParsedExpression::IntegerLiteral { value, range }),
            Err(_) => Err(Error::new_with_range(
                ErrorType::Parser,
                format!("Failed to parse '{}' to int", value),
                range,
            )),
        }
    }

    fn parse_constant(&mut self) -> Result<ParsedExpression> {
        let constant_start = self.current_index(false);
        let token = self.consume()?;
        let range = (constant_start..self.current_index(true)).into();
        match token.token_type {
            TokenType::True => Ok(ParsedExpression::BooleanLiteral { value: true, range }),
            TokenType::False => Ok(ParsedExpression::BooleanLiteral {
                value: false,
                range,
            }),
            TokenType::IntegerLiteral => Self::parse_integer_literal(token.value, "", 10, range),
            TokenType::BinaryIntegerLiteral => {
                Self::parse_integer_literal(token.value, "0b", 2, range)
            }
            TokenType::OctalIntegerLiteral => {
                Self::parse_integer_literal(token.value, "0o", 8, range)
            }
            TokenType::HexIntegerLiteral => {
                Self::parse_integer_literal(token.value, "0x", 16, range)
            }
            TokenType::StringLiteral => Ok(ParsedExpression::StringLiteral {
                value: token.value[1..token.value.len() - 1]
                    .to_string()
                    .replace("\\n", "\n")
                    .replace("\\t", "\t")
                    .replace("\\r", "\r")
                    .replace("\\\"", "\""),
                range,
            }),
            _ => Err(Error::new_with_range(
                ErrorType::Parser,
                format!("Parsing constant but got token {:?}", token.token_type),
                token.range,
            )),
        }
    }

    fn parse_parenthesized(&mut self) -> Result<ParsedExpression> {
        self.consume_assert(TokenType::LeftParen)?;
        let expr = self.parse_expression(0)?;
        self.consume_assert(TokenType::RightParen)?;
        Ok(expr)
    }

    fn parse_binary_operator(
        &mut self,
        start_index: usize,
        left: ParsedExpression,
        precedence: usize,
    ) -> Result<ParsedExpression> {
        let operator = self.consume()?;

        let op_type = BinaryOperatorType::from_token_type(operator.token_type);
        let right = self.parse_expression(precedence)?;
        Ok(ParsedExpression::BinaryOperator {
            op_type,
            left: Box::new(left),
            right: Box::new(right),
            range: (start_index..self.current_index(true)).into(),
        })
    }

    fn parse_expression(&mut self, precedence: usize) -> Result<ParsedExpression> {
        let exit_tokens = vec![
            TokenType::SemiColon,
            TokenType::RightParen,
            TokenType::LeftBrace,
            TokenType::Comma,
            TokenType::Equals,
        ];

        let token_type = self.peek()?.token_type;
        let start_index = self.current_index(false);

        let mut left = match self.prefix_fns.get(&token_type) {
            Some(func) => func(self),
            None => Err(Error::new_with_range(
                ErrorType::Parser,
                format!(
                    "Did not expect token '{:?}' while parsing prefix expression",
                    token_type
                ),
                self.peek()?.range,
            )),
        }?;

        if self.eof() {
            return Ok(left);
        }

        let token_type = self.peek()?.token_type;

        if let Some(func) = self.postfix_fns.get(&token_type) {
            left = func(self, left)?;
        }

        if self.eof() || exit_tokens.contains(&self.peek()?.token_type) {
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
                    format!(
                        "Did not expect token '{:?}' while parsing infix expression",
                        token_type
                    ),
                    self.peek()?.range,
                )),
            }?;
        }

        Ok(left)
    }

    fn parse_return_statement(&mut self) -> Result<ParsedStatement> {
        let return_start = self.current_index(false);
        self.consume_assert(TokenType::Return)?;

        let value = self.parse_expression(0)?;
        self.consume_assert(TokenType::SemiColon)?;

        Ok(ParsedStatement::Return {
            value,
            range: (return_start..self.current_index(true)).into(),
        })
    }

    fn parse_let_statement(&mut self) -> Result<ParsedStatement> {
        let let_start = self.current_index(false);
        self.consume_assert(TokenType::Let)?;
        let (name, value_type) = self.parse_identifier_type()?;

        if self.peek()?.token_type == TokenType::Equals {
            // We have a declaration and assignment combined
            self.consume_assert(TokenType::Equals)?;
            let decl_start = self.current_index(false);
            let expr = self.parse_expression(0)?;
            self.consume_assert(TokenType::SemiColon)?;

            return Ok(ParsedStatement::Block {
                children: vec![
                    ParsedStatement::Declaration {
                        name: name.clone(),
                        value_type,
                        range: (let_start..decl_start).into(),
                    },
                    ParsedStatement::Assignment {
                        left: ParsedExpression::VariableRef {
                            name,
                            range: (decl_start..self.current_index(true)).into(),
                        },
                        right: expr,
                        range: (decl_start..self.current_index(true)).into(),
                    },
                ],
                scoped: false,
                range: (let_start..self.current_index(true)).into(),
            });
        }

        self.consume_assert(TokenType::SemiColon)?;

        Ok(ParsedStatement::Declaration {
            name,
            value_type,
            range: (let_start..self.current_index(true)).into(),
        })
    }

    fn parse_assignment_statement(&mut self) -> Result<ParsedStatement> {
        let assignment_start = self.current_index(false);
        let left = self.parse_expression(0)?;
        self.consume_assert(TokenType::Equals)?;

        let right = self.parse_expression(0)?;
        self.consume_assert(TokenType::SemiColon)?;

        Ok(ParsedStatement::Assignment {
            left,
            right,
            range: (assignment_start..self.current_index(true)).into(),
        })
    }

    fn parse_while_statement(&mut self) -> Result<ParsedStatement> {
        let while_start = self.current_index(false);
        self.consume_assert(TokenType::While)?;

        let condition = self.parse_expression(0)?;
        let statement = self.parse_statement()?;

        Ok(ParsedStatement::While {
            condition,
            body: Box::new(statement),
            range: (while_start..self.current_index(true)).into(),
        })
    }

    fn parse_if_statement(&mut self) -> Result<ParsedStatement> {
        let if_start = self.current_index(false);
        self.consume_assert(TokenType::If)?;

        let condition = self.parse_expression(0)?;
        let statement = self.parse_statement()?;

        Ok(ParsedStatement::If {
            condition,
            body: Box::new(statement),
            range: (if_start..self.current_index(true)).into(),
        })
    }

    fn parse_identifier_type(&mut self) -> Result<(String, ParsedType)> {
        let identifier = self.consume_assert(TokenType::Identifier)?.value;
        self.consume_assert(TokenType::Colon)?;

        let value_type = self.parse_type()?;

        Ok((identifier.to_string(), value_type))
    }

    fn parse_annotation(&mut self) -> Result<(String, Option<ParsedExpression>)> {
        self.consume_assert(TokenType::At)?;
        let name = self.consume_assert(TokenType::Identifier)?;

        if self.peek()?.token_type != TokenType::LeftParen {
            return Ok((name.value.to_string(), None));
        }

        self.consume_assert(TokenType::LeftParen)?;
        let value = self.parse_expression(0)?;
        self.consume_assert(TokenType::RightParen)?;

        Ok((name.value.to_string(), Some(value)))
    }

    fn parse_function(&mut self) -> Result<ParsedUpperStatement> {
        let mut annotations = vec![];

        while self.peek()?.token_type == TokenType::At {
            let annotation = self.parse_annotation()?;
            annotations.push(annotation);
        }

        let function_start = self.current_index(false);
        self.consume_assert(TokenType::Fn)?;

        let name = self
            .consume_assert(TokenType::Identifier)?
            .value
            .to_string();
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
            ParsedType::Named("void".to_string())
        } else {
            self.parse_type()?
        };

        let body = self.parse_statement()?;
        Ok(ParsedUpperStatement::Function {
            name,
            parameters,
            return_type,
            body,
            annotations,
            range: (function_start..self.current_index(true)).into(),
        })
    }

    fn parse_statement(&mut self) -> Result<ParsedStatement> {
        let token = self.peek()?;

        match token.token_type {
            TokenType::Return => self.parse_return_statement(),
            TokenType::LeftBrace => {
                let block_start = self.current_index(false);
                self.consume_assert(TokenType::LeftBrace)?;
                let mut children = vec![];
                while self.peek()?.token_type != TokenType::RightBrace {
                    children.push(self.parse_statement()?);
                }

                self.consume_assert(TokenType::RightBrace)?;

                Ok(ParsedStatement::Block {
                    children,
                    scoped: true,
                    range: (block_start..self.current_index(true)).into(),
                })
            }
            TokenType::Let => self.parse_let_statement(),
            TokenType::While => self.parse_while_statement(),
            TokenType::If => self.parse_if_statement(),
            TokenType::Identifier => match self.peeks(1)?.token_type {
                TokenType::LeftParen => {
                    let expr_start = self.current_index(false);
                    let result = ParsedStatement::Expression {
                        expr: self.parse_expression(0)?,
                        range: (expr_start..self.current_index(true) + 1).into(),
                    };
                    self.consume_assert(TokenType::SemiColon)?;
                    Ok(result)
                }
                _ => self.parse_assignment_statement(),
            },
            _ => self.parse_assignment_statement(),
        }
    }

    fn parse_const_declaration(&mut self) -> Result<ParsedUpperStatement> {
        let start_index = self.current_index(false);

        self.consume_assert(TokenType::Const)?;
        let name = self
            .consume_assert(TokenType::Identifier)?
            .value
            .to_string();

        self.consume_assert(TokenType::Colon)?;
        let value_type = self.parse_type()?;

        self.consume_assert(TokenType::Equals)?;
        let value = self.parse_expression(0)?;

        self.consume_assert(TokenType::SemiColon)?;

        Ok(ParsedUpperStatement::ConstDeclaration {
            name,
            value_type,
            value,
            range: (start_index..self.current_index(true)).into(),
        })
    }

    fn parse_extern(&mut self) -> Result<ParsedUpperStatement> {
        let start_index = self.current_index(false);

        self.consume_assert(TokenType::Extern)?;
        let symbol = self.consume_assert(TokenType::StringLiteral)?.value;
        self.consume_assert(TokenType::SemiColon)?;

        Ok(ParsedUpperStatement::ExternDeclaration {
            name: symbol[1..symbol.len() - 1].to_string(),
            range: (start_index..self.current_index(true)).into(),
        })
    }

    fn parse_struct_declaration(&mut self) -> Result<ParsedUpperStatement> {
        self.consume_assert(TokenType::Struct)?;
        let name = self
            .consume_assert(TokenType::Identifier)?
            .value
            .to_string();

        self.consume_assert(TokenType::LeftBrace)?;

        let mut fields = vec![];

        while self.peek()?.token_type != TokenType::RightBrace {
            let field_name = self
                .consume_assert(TokenType::Identifier)?
                .value
                .to_string();

            self.consume_assert(TokenType::Colon)?;

            let field_type = self.parse_type()?;

            self.consume_assert(TokenType::Comma)?;

            fields.push((field_name, field_type));
        }

        self.consume_assert(TokenType::RightBrace)?;

        Ok(ParsedUpperStatement::StructDeclaration { name, fields })
    }

    pub fn parse_upper_statement(&mut self) -> Result<ParsedUpperStatement> {
        let token = self.peek()?;

        match token.token_type {
            TokenType::Extern => self.parse_extern(),
            TokenType::Struct => self.parse_struct_declaration(),
            TokenType::Fn | TokenType::At => self.parse_function(),
            TokenType::Const => self.parse_const_declaration(),
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

impl<'a> Iterator for Parser<'a> {
    type Item = Result<ParsedUpperStatement>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.eof() {
            true => None,
            false => Some(self.parse_upper_statement()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ast::BinaryOperatorType, tokenizer::tokenize};

    fn parse_statements(input: &str) -> Result<Vec<ParsedStatement>> {
        let tokens = tokenize(input)?;

        let mut parser = Parser::new(&tokens);
        let mut result = vec![];
        while !parser.eof() {
            result.push(parser.parse_statement()?);
        }

        Ok(result)
    }

    fn parse_expressions(input: &str) -> Result<Vec<ParsedExpression>> {
        let tokens = tokenize(input)?;

        let mut parser = Parser::new(&tokens);
        let mut result = vec![];
        while !parser.eof() {
            result.push(parser.parse_expression(0)?);
        }

        Ok(result)
    }

    fn parse_function(input: &str) -> Result<ParsedUpperStatement> {
        let tokens = tokenize(input)?;

        let mut parser = Parser::new(&tokens);
        parser.parse_function()
    }

    #[test]
    fn parse_constant() -> Result<()> {
        let exprs = parse_expressions("123")?;

        assert_eq!(
            exprs[0],
            ParsedExpression::IntegerLiteral {
                value: 123,
                range: (0..3).into()
            }
        );

        Ok(())
    }

    #[test]
    fn parse_unary_op() -> Result<()> {
        let exprs = parse_expressions("-123")?;

        assert_eq!(
            exprs[0],
            ParsedExpression::UnaryOperator {
                op_type: UnaryOperatorType::Negate,
                expr: Box::new(ParsedExpression::IntegerLiteral {
                    value: 123,
                    range: (1..4).into()
                }),
                range: (0..4).into()
            }
        );

        Ok(())
    }

    #[test]
    fn parse_binary_op() -> Result<()> {
        let exprs = parse_expressions("456 - 123")?;

        assert_eq!(
            exprs[0],
            ParsedExpression::BinaryOperator {
                op_type: BinaryOperatorType::Subtract,
                left: Box::new(ParsedExpression::IntegerLiteral {
                    value: 456,
                    range: (0..3).into()
                }),
                right: Box::new(ParsedExpression::IntegerLiteral {
                    value: 123,
                    range: (6..9).into()
                }),
                range: (0..9).into()
            }
        );

        Ok(())
    }

    #[test]
    fn parse_binop_precedence_1() -> Result<()> {
        let exprs = parse_expressions("456 - 123 * 789")?;

        assert_eq!(
            exprs[0],
            ParsedExpression::BinaryOperator {
                op_type: BinaryOperatorType::Subtract,
                left: Box::new(ParsedExpression::IntegerLiteral {
                    value: 456,
                    range: (0..3).into()
                }),
                right: Box::new(ParsedExpression::BinaryOperator {
                    op_type: BinaryOperatorType::Multiply,
                    left: Box::new(ParsedExpression::IntegerLiteral {
                        value: 123,
                        range: (6..9).into()
                    }),
                    right: Box::new(ParsedExpression::IntegerLiteral {
                        value: 789,
                        range: (12..15).into()
                    }),
                    range: (6..15).into()
                }),
                range: (0..15).into(),
            }
        );

        Ok(())
    }

    #[test]
    fn parse_binop_precedence_2() -> Result<()> {
        let exprs = parse_expressions("456 * 123 - 789")?;

        assert_eq!(
            exprs[0],
            ParsedExpression::BinaryOperator {
                op_type: BinaryOperatorType::Subtract,
                left: Box::new(ParsedExpression::BinaryOperator {
                    op_type: BinaryOperatorType::Multiply,
                    left: Box::new(ParsedExpression::IntegerLiteral {
                        value: 456,
                        range: (0..3).into()
                    }),
                    right: Box::new(ParsedExpression::IntegerLiteral {
                        value: 123,
                        range: (6..9).into()
                    }),
                    range: (0..9).into()
                }),
                right: Box::new(ParsedExpression::IntegerLiteral {
                    value: 789,
                    range: (12..15).into()
                }),
                range: (0..15).into(),
            }
        );

        Ok(())
    }

    #[test]
    fn parse_return_statement() -> Result<()> {
        let stmts = parse_statements("return 456 - 123;")?;

        assert_eq!(
            stmts[0],
            ParsedStatement::Return {
                value: ParsedExpression::BinaryOperator {
                    op_type: BinaryOperatorType::Subtract,
                    left: Box::new(ParsedExpression::IntegerLiteral {
                        value: 456,
                        range: (7..10).into()
                    }),
                    right: Box::new(ParsedExpression::IntegerLiteral {
                        value: 123,
                        range: (13..16).into()
                    }),
                    range: (7..16).into()
                },
                range: (0..17).into()
            }
        );

        Ok(())
    }

    #[test]
    fn parse_empty_block() -> Result<()> {
        let stmts = parse_statements("{}")?;
        assert_eq!(stmts.len(), 1);
        assert_eq!(
            stmts[0],
            ParsedStatement::Block {
                children: vec![],
                scoped: true,
                range: (0..2).into()
            }
        );

        Ok(())
    }

    #[test]
    fn parse_nested_block() -> Result<()> {
        let stmts = parse_statements("{{{}}}")?;
        assert_eq!(stmts.len(), 1);
        assert_eq!(
            stmts[0],
            ParsedStatement::Block {
                children: vec![ParsedStatement::Block {
                    children: vec![ParsedStatement::Block {
                        children: vec![],
                        scoped: true,
                        range: (2..4).into()
                    }],
                    scoped: true,
                    range: (1..5).into()
                }],
                scoped: true,
                range: (0..6).into()
            }
        );
        Ok(())
    }

    #[test]
    fn parse_simple_function() -> Result<()> {
        let func = parse_function("fn test() { return 12; }")?;
        assert_eq!(
            func,
            ParsedUpperStatement::Function {
                name: "test".to_string(),
                parameters: vec![],
                return_type: ParsedType::Named("void".to_string()),
                body: ParsedStatement::Block {
                    children: vec![ParsedStatement::Return {
                        value: ParsedExpression::IntegerLiteral {
                            value: 12,
                            range: (19..21).into()
                        },
                        range: (12..22).into()
                    }],
                    scoped: true,
                    range: (10..24).into()
                },
                annotations: vec![],
                range: (0..24).into()
            }
        );
        Ok(())
    }

    #[test]
    fn parse_function_return_type() -> Result<()> {
        let func = parse_function("fn test() u8 { return 12; }")?;
        assert_eq!(
            func,
            ParsedUpperStatement::Function {
                name: "test".to_string(),
                parameters: vec![],
                return_type: ParsedType::Named("u8".to_string()),
                body: ParsedStatement::Block {
                    children: vec![ParsedStatement::Return {
                        value: ParsedExpression::IntegerLiteral {
                            value: 12,
                            range: (22..24).into()
                        },
                        range: (15..25).into()
                    }],
                    scoped: true,
                    range: (13..27).into()
                },
                annotations: vec![],
                range: (0..27).into()
            }
        );
        Ok(())
    }

    #[test]
    fn parse_function_call_expr() -> Result<()> {
        let call = parse_expressions("test()")?;
        assert_eq!(
            call[0],
            ParsedExpression::FunctionCall {
                name: "test".to_string(),
                arguments: vec![],
                range: (0..6).into()
            }
        );
        Ok(())
    }

    #[test]
    fn parse_string_constant_expr() -> Result<()> {
        let string = parse_expressions("\"test\"")?;
        assert_eq!(
            string[0],
            ParsedExpression::StringLiteral {
                value: "test".to_string(),
                range: (0..6).into()
            }
        );
        Ok(())
    }

    #[test]
    fn parse_string_constant_escaped_expr() -> Result<()> {
        let string = parse_expressions("\"test\\t\\'test\\'\\n\"")?;
        assert_eq!(
            string[0],
            ParsedExpression::StringLiteral {
                value: "test\t\\'test\\'\n".to_string(),
                range: (0..18).into()
            }
        );
        Ok(())
    }

    #[test]
    fn parse_function_call_stmt() -> Result<()> {
        let call = parse_statements("test();")?;
        assert_eq!(
            call[0],
            ParsedStatement::Expression {
                expr: ParsedExpression::FunctionCall {
                    name: "test".to_string(),
                    arguments: vec![],
                    range: (0..6).into()
                },
                range: (0..7).into()
            }
        );
        Ok(())
    }

    #[test]
    fn parse_function_call_stmt_with_arg() -> Result<()> {
        let call = parse_statements("test(x);")?;
        assert_eq!(
            call[0],
            ParsedStatement::Expression {
                expr: ParsedExpression::FunctionCall {
                    name: "test".to_string(),
                    arguments: vec![ParsedExpression::VariableRef {
                        name: "x".to_string(),
                        range: (5..6).into()
                    }],
                    range: (0..7).into(),
                },
                range: (0..8).into()
            }
        );
        Ok(())
    }
}
