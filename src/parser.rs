use std::collections::HashMap;
use std::str::FromStr;

use crate::{
    ast::{BinaryOperatorType, Expression, Statement, UnaryOperatorType},
    error::{Error, ErrorType, Result},
    tokenizer::{Token, TokenType},
    types::Type,
};

type PrefixParseFn<'a, C> = fn(&mut Parser<'a, C>) -> Result<Expression<C>>;
type InfixParseFn<'a, C> =
    fn(&mut Parser<'a, C>, left: Expression<C>, precedence: usize) -> Result<Expression<C>>;

pub struct Parser<'a, C> {
    tokens: &'a [Token<'a>],
    index: usize,
    prefix_fns: HashMap<TokenType, PrefixParseFn<'a, C>>,
    infix_fns: HashMap<TokenType, (InfixParseFn<'a, C>, usize)>,
    source_file: &'a str,
}

pub trait Literal {
    fn get_bits(&self) -> usize;
}

impl Literal for u64 {
    fn get_bits(&self) -> usize {
        if *self <= 255 {
            8
        } else if *self <= 65535 {
            16
        } else if *self <= 4294967295 {
            32
        } else {
            64
        }
    }
}

impl<'a, C: FromStr + Literal> Parser<'a, C> {
    pub fn new(tokens: &'a [Token], source_file: &'a str) -> Self {
        let mut prefix_fns: HashMap<_, PrefixParseFn<'a, C>> = HashMap::new();
        prefix_fns.insert(
            TokenType::Identifier,
            Self::parse_identifier_or_function_call,
        );
        prefix_fns.insert(TokenType::Minus, Self::parse_unary_expression);
        prefix_fns.insert(TokenType::Ampersand, Self::parse_unary_expression);
        prefix_fns.insert(TokenType::Asterisk, Self::parse_unary_expression);
        prefix_fns.insert(TokenType::IntegerLiteral, Self::parse_constant);
        prefix_fns.insert(TokenType::StringLiteral, Self::parse_constant);
        prefix_fns.insert(TokenType::LeftParen, Self::parse_parenthesized);

        let mut infix_fns: HashMap<_, (InfixParseFn<'a, C>, usize)> = HashMap::new();
        infix_fns.insert(TokenType::Plus, (Self::parse_binary_operator, 3));
        infix_fns.insert(TokenType::Minus, (Self::parse_binary_operator, 3));
        infix_fns.insert(TokenType::Asterisk, (Self::parse_binary_operator, 4));
        infix_fns.insert(TokenType::Slash, (Self::parse_binary_operator, 4));
        infix_fns.insert(TokenType::DoubleEqual, (Self::parse_binary_operator, 2));
        infix_fns.insert(TokenType::NotEqual, (Self::parse_binary_operator, 2));
        infix_fns.insert(TokenType::LessThan, (Self::parse_binary_operator, 2));
        infix_fns.insert(TokenType::LessThanEqual, (Self::parse_binary_operator, 2));
        infix_fns.insert(TokenType::GreaterThan, (Self::parse_binary_operator, 2));
        infix_fns.insert(
            TokenType::GreaterThanEqual,
            (Self::parse_binary_operator, 2),
        );

        Self {
            tokens,
            index: 0,
            prefix_fns,
            infix_fns,
            source_file,
        }
    }

    pub fn eof(&self) -> bool {
        self.index >= self.tokens.len()
    }

    fn peek(&self) -> Result<&'a Token<'a>> {
        if self.eof() {
            Err(Error::new(
                ErrorType::Parser,
                "Token stream out of bounds".to_string(),
                self.tokens.last().unwrap().range.clone(),
                self.source_file.to_string(),
            ))
        } else {
            Ok(&self.tokens[self.index])
        }
    }

    fn peeks(&self, i: usize) -> Result<&'a Token<'a>> {
        if self.index + i >= self.tokens.len() {
            Err(Error::new(
                ErrorType::Parser,
                "Token stream out of bounds".to_string(),
                self.tokens.last().unwrap().range.clone(),
                self.source_file.to_string(),
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
            return Err(Error::new(
                ErrorType::Parser,
                format!(
                    "Expected token {:?}, but got {:?}",
                    token_type, token.token_type
                ),
                token.range.clone(),
                self.source_file.to_string(),
            ));
        }
        Ok(token)
    }

    fn parse_type(&mut self) -> Result<Type> {
        let token = self.consume()?;
        let mut result = match token.token_type {
            TokenType::UInt8 => Ok(Type::UInt8()),
            TokenType::UInt16 => Ok(Type::UInt16()),
            TokenType::UInt32 => Ok(Type::UInt32()),
            TokenType::UInt64 => Ok(Type::UInt64()),
            TokenType::Bool => Ok(Type::Bool()),
            _ => Err(Error::new(
                ErrorType::Parser,
                format!("Parsing type but got token {:?}", token.token_type),
                token.range.clone(),
                self.source_file.to_string(),
            )),
        };

        if self.peek()?.token_type == TokenType::Asterisk {
            self.consume()?;
            result = Ok(result?.get_ref());
        }

        result
    }

    fn parse_function_call(&mut self) -> Result<Expression<C>> {
        let function_start = self.index;
        let name = self.consume_assert(TokenType::Identifier)?.value;
        self.consume_assert(TokenType::LeftParen)?;

        let mut args = vec![];
        loop {
            if self.peek()?.token_type == TokenType::RightParen {
                break;
            }

            args.push(self.parse_expression(0)?);

            if self.peek()?.token_type == TokenType::RightParen {
                break;
            }

            self.consume_assert(TokenType::Comma)?;
        }

        self.consume_assert(TokenType::RightParen)?;
        Ok(Expression::FunctionCall(
            name.to_string(),
            args,
            function_start..self.index,
        ))
    }

    fn parse_identifier_or_function_call(&mut self) -> Result<Expression<C>> {
        let identifier_start = self.index;
        if self.peeks(1)?.token_type == TokenType::LeftParen {
            self.parse_function_call()
        } else {
            let name = self.consume_assert(TokenType::Identifier)?.value;
            Ok(Expression::VariableRef(
                name.to_string(),
                identifier_start..self.index,
            ))
        }
    }

    fn parse_unary_expression(&mut self) -> Result<Expression<C>> {
        let unary_start = self.index;
        let token = self.consume()?;
        let unop_type = UnaryOperatorType::from_token_type(token.token_type);
        let expression = self.parse_expression(0)?;
        Ok(Expression::UnaryOperator(
            unop_type,
            Box::new(expression),
            unary_start..self.index,
        ))
    }

    fn parse_constant(&mut self) -> Result<Expression<C>> {
        let constant_start = self.index;
        let token = self.consume()?;
        match token.token_type {
            TokenType::IntegerLiteral => match token.value.parse::<C>() {
                Ok(value) => {
                    let bits = value.get_bits();
                    match bits {
                        8 => Ok(Expression::Literal(
                            value,
                            Type::UInt8(),
                            constant_start..self.index,
                        )),
                        16 => Ok(Expression::Literal(
                            value,
                            Type::UInt16(),
                            constant_start..self.index,
                        )),
                        32 => Ok(Expression::Literal(
                            value,
                            Type::UInt32(),
                            constant_start..self.index,
                        )),
                        64 => Ok(Expression::Literal(
                            value,
                            Type::UInt64(),
                            constant_start..self.index,
                        )),
                        _ => Err(Error::new(
                            ErrorType::Parser,
                            format!(
                                "Integer literal is too big to fit into 64 bits ({}): {}",
                                bits, token.value
                            ),
                            token.range.clone(),
                            self.source_file.to_string(),
                        )),
                    }
                }
                Err(_) => Err(Error::new(
                    ErrorType::Parser,
                    format!("Failed to parse '{}' to int", token.value),
                    token.range.clone(),
                    self.source_file.to_string(),
                )),
            },
            TokenType::StringLiteral => Ok(Expression::StringLiteral(
                token.value[1..token.value.len() - 1]
                    .to_string()
                    .replace("\\n", "\n")
                    .replace("\\t", "\t")
                    .replace("\\r", "\r")
                    .replace("\\\"", "\""),
                constant_start..self.index,
            )),
            _ => Err(Error::new(
                ErrorType::Parser,
                format!("Parsing constant but got token {:?}", token.token_type),
                token.range.clone(),
                self.source_file.to_string(),
            )),
        }
    }

    fn parse_parenthesized(&mut self) -> Result<Expression<C>> {
        self.consume_assert(TokenType::LeftParen)?;
        let expr = self.parse_expression(0)?;
        self.consume_assert(TokenType::RightParen)?;
        Ok(expr)
    }

    fn parse_binary_operator(
        &mut self,
        left: Expression<C>,
        precedence: usize,
    ) -> Result<Expression<C>> {
        let binop_start = self.index;
        let operator = self.consume()?;

        let op_type = BinaryOperatorType::from_token_type(operator.token_type);
        let right = self.parse_expression(precedence)?;
        Ok(Expression::BinaryOperator(
            op_type,
            Box::new(left),
            Box::new(right),
            binop_start..self.index,
        ))
    }

    pub fn parse_expression(&mut self, precedence: usize) -> Result<Expression<C>> {
        let exit_tokens = vec![
            TokenType::SemiColon,
            TokenType::RightParen,
            TokenType::LeftBrace,
            TokenType::Comma,
        ];

        let token_type = self.peek()?.token_type;

        let mut left = match self.prefix_fns.get(&token_type) {
            Some(func) => func(self),
            None => Err(Error::new(
                ErrorType::Parser,
                format!(
                    "Did not expect token '{:?}' while parsing prefix expression",
                    token_type
                ),
                self.peek()?.range.clone(),
                self.source_file.to_string(),
            )),
        }?;

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
                    func(self, left, prec)
                }
                None => Err(Error::new(
                    ErrorType::Parser,
                    format!(
                        "Did not expect token '{:?}' while parsing infix expression",
                        token_type
                    ),
                    self.peek()?.range.clone(),
                    self.source_file.to_string(),
                )),
            }?;
        }

        Ok(left)
    }

    fn parse_return_statement(&mut self) -> Result<Statement<C>> {
        let return_start = self.index;
        self.consume_assert(TokenType::Return)?;
        let expr = self.parse_expression(0)?;
        self.consume_assert(TokenType::SemiColon)?;
        Ok(Statement::Return(expr, return_start..self.index))
    }

    fn parse_let_statement(&mut self) -> Result<Statement<C>> {
        let let_start = self.index;
        self.consume_assert(TokenType::Let)?;
        let (variable_name, value_type) = self.parse_identifier_type()?;

        if self.peek()?.token_type == TokenType::Equals {
            // We have a declaration and assignment combined
            self.consume_assert(TokenType::Equals)?;
            let decl_start = self.index;
            let expr = self.parse_expression(0)?;
            self.consume_assert(TokenType::SemiColon)?;

            return Ok(Statement::Block(
                vec![
                    Statement::Declaration(
                        variable_name.clone(),
                        value_type,
                        let_start..decl_start,
                    ),
                    Statement::Assignment(variable_name, expr, decl_start..self.index),
                ],
                false,
                let_start..self.index,
            ));
        }

        self.consume_assert(TokenType::SemiColon)?;
        Ok(Statement::Declaration(
            variable_name,
            value_type,
            let_start..self.index,
        ))
    }

    fn parse_assignment_statement(&mut self) -> Result<Statement<C>> {
        let assignment_start = self.index;
        let name = self.consume_assert(TokenType::Identifier)?.value;
        self.consume_assert(TokenType::Equals)?;
        let expr = self.parse_expression(0)?;
        self.consume_assert(TokenType::SemiColon)?;
        Ok(Statement::Assignment(
            name.to_string(),
            expr,
            assignment_start..self.index,
        ))
    }

    fn parse_while_statement(&mut self) -> Result<Statement<C>> {
        let while_start = self.index;
        self.consume_assert(TokenType::While)?;
        let expr = self.parse_expression(0)?;
        let statement = self.parse_statement()?;
        Ok(Statement::While(
            expr,
            Box::new(statement),
            while_start..self.index,
        ))
    }

    fn parse_if_statement(&mut self) -> Result<Statement<C>> {
        let if_start = self.index;
        self.consume_assert(TokenType::If)?;
        let expr = self.parse_expression(0)?;
        let statement = self.parse_statement()?;
        Ok(Statement::If(
            expr,
            Box::new(statement),
            if_start..self.index,
        ))
    }

    fn parse_identifier_type(&mut self) -> Result<(String, Type)> {
        let identifier = self.consume_assert(TokenType::Identifier)?.value;
        self.consume_assert(TokenType::Colon)?;
        let value_type = self.parse_type()?;
        Ok((identifier.to_string(), value_type))
    }

    fn parse_function(&mut self) -> Result<Statement<C>> {
        let function_start = self.index;
        self.consume_assert(TokenType::Fn)?;
        let identifier = self.consume_assert(TokenType::Identifier)?;
        self.consume_assert(TokenType::LeftParen)?;
        let mut args = vec![];

        loop {
            if self.peek()?.token_type == TokenType::RightParen {
                break;
            }

            let (name, value_type) = self.parse_identifier_type()?;
            args.push((name, value_type));

            if self.peek()?.token_type == TokenType::RightParen {
                break;
            }

            self.consume_assert(TokenType::Comma)?;
        }

        self.consume_assert(TokenType::RightParen)?;

        let mut return_type = if self.peek()?.token_type == TokenType::LeftBrace {
            Type::Void()
        } else {
            self.parse_type()?
        };

        if self.peek()?.token_type == TokenType::Colon {
            self.consume_assert(TokenType::Colon)?;
            return_type = self.parse_type()?;
        }

        let body = self.parse_statement()?;
        Ok(Statement::Function(
            identifier.value.to_string(),
            args,
            return_type,
            Box::new(body),
            function_start..self.index,
        ))
    }

    pub fn parse_statement(&mut self) -> Result<Statement<C>> {
        let token = self.peek()?;

        match token.token_type {
            TokenType::Return => self.parse_return_statement(),
            TokenType::LeftBrace => {
                let block_start = self.index;
                self.consume_assert(TokenType::LeftBrace)?;
                let mut statements = vec![];
                while self.peek()?.token_type != TokenType::RightBrace {
                    statements.push(self.parse_statement()?);
                }

                self.consume_assert(TokenType::RightBrace)?;

                Ok(Statement::Block(statements, true, block_start..self.index))
            }
            TokenType::Let => self.parse_let_statement(),
            TokenType::While => self.parse_while_statement(),
            TokenType::If => self.parse_if_statement(),
            TokenType::Identifier => match self.peeks(1)?.token_type {
                TokenType::Equals => self.parse_assignment_statement(),
                TokenType::LeftParen => {
                    let expr_start = self.index;
                    let result =
                        Statement::Expression(self.parse_expression(0)?, expr_start..self.index);
                    self.consume_assert(TokenType::SemiColon)?;
                    Ok(result)
                }
                _ => Err(Error::new(
                    ErrorType::Parser,
                    format!("Unexpected token {:?}", token.token_type),
                    token.range.clone(),
                    self.source_file.to_string(),
                )),
            },
            TokenType::Fn => self.parse_function(),
            _ => Err(Error::new(
                ErrorType::Parser,
                format!("Unexpected token {:?}", token.token_type),
                token.range.clone(),
                self.source_file.to_string(),
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ast::BinaryOperatorType, tokenizer::tokenize};

    fn parse_statements(input: &str) -> Result<Vec<Statement<u64>>> {
        let tokens = tokenize(input, "test.dodo")?;

        let mut parser = Parser::new(&tokens, "test.dodo");
        let mut result = vec![];
        while !parser.eof() {
            result.push(parser.parse_statement()?);
        }

        Ok(result)
    }

    fn parse_expressions(input: &str) -> Result<Vec<Expression<u64>>> {
        let tokens = tokenize(input, "test.dodo")?;

        let mut parser = Parser::new(&tokens, "test.dodo");
        let mut result = vec![];
        while !parser.eof() {
            result.push(parser.parse_expression(0)?);
        }

        Ok(result)
    }

    fn parse_function(input: &str) -> Result<Statement<u64>> {
        let tokens = tokenize(input, "test.dodo")?;

        let mut parser = Parser::new(&tokens, "test.dodo");
        parser.parse_function()
    }

    #[test]
    fn parse_constant() -> Result<()> {
        let exprs = parse_expressions("123")?;

        assert_eq!(exprs[0], Expression::Literal(123, Type::UInt8(), 0..3));

        Ok(())
    }

    #[test]
    fn parse_unary_op() -> Result<()> {
        let exprs = parse_expressions("-123")?;

        assert_eq!(
            exprs[0],
            Expression::UnaryOperator(
                UnaryOperatorType::Negate,
                Box::new(Expression::Literal(123, Type::UInt8(), 1..4)),
                0..4
            )
        );

        Ok(())
    }

    #[test]
    fn parse_binary_op() -> Result<()> {
        let exprs = parse_expressions("456 - 123")?;

        assert_eq!(
            exprs[0],
            Expression::BinaryOperator(
                BinaryOperatorType::Subtract,
                Box::new(Expression::Literal(456, Type::UInt16(), 0..3)),
                Box::new(Expression::Literal(123, Type::UInt8(), 7..10)),
                0..10
            )
        );

        Ok(())
    }

    #[test]
    fn parse_binop_precedence_1() -> Result<()> {
        let exprs = parse_expressions("456 - 123 * 789")?;

        assert_eq!(
            exprs[0],
            Expression::BinaryOperator(
                BinaryOperatorType::Subtract,
                Box::new(Expression::Literal(456, Type::UInt16(), 0..1)),
                Box::new(Expression::BinaryOperator(
                    BinaryOperatorType::Multiply,
                    Box::new(Expression::Literal(123, Type::UInt8(), 0..1)),
                    Box::new(Expression::Literal(789, Type::UInt16(), 0..1)),
                    0..1
                )),
                0..1,
            )
        );

        Ok(())
    }

    #[test]
    fn parse_binop_precedence_2() -> Result<()> {
        let exprs = parse_expressions("456 * 123 - 789")?;

        assert_eq!(
            exprs[0],
            Expression::BinaryOperator(
                BinaryOperatorType::Subtract,
                Box::new(Expression::BinaryOperator(
                    BinaryOperatorType::Multiply,
                    Box::new(Expression::Literal(456, Type::UInt16(), 0..1)),
                    Box::new(Expression::Literal(123, Type::UInt8(), 0..1)),
                    0..1
                )),
                Box::new(Expression::Literal(789, Type::UInt16(), 0..1)),
                0..1
            )
        );

        Ok(())
    }

    #[test]
    fn parse_return_statement() -> Result<()> {
        let stmts = parse_statements("return 456 - 123;")?;

        assert_eq!(
            stmts[0],
            Statement::Return(
                Expression::BinaryOperator(
                    BinaryOperatorType::Subtract,
                    Box::new(Expression::Literal(456, Type::UInt16(), 0..1)),
                    Box::new(Expression::Literal(123, Type::UInt8(), 0..1)),
                    0..1
                ),
                0..1
            )
        );

        Ok(())
    }

    #[test]
    fn parse_empty_block() -> Result<()> {
        let stmts = parse_statements("{}")?;
        assert_eq!(stmts.len(), 1);
        assert_eq!(stmts[0], Statement::Block(vec![], true, 0..2));

        Ok(())
    }

    #[test]
    fn parse_nested_block() -> Result<()> {
        let stmts = parse_statements("{{{}}}")?;
        assert_eq!(stmts.len(), 1);
        assert_eq!(
            stmts[0],
            Statement::Block(
                vec![Statement::Block(
                    vec![Statement::Block(vec![], true, 3..5)],
                    true,
                    2..6
                )],
                true,
                1..7
            )
        );
        Ok(())
    }

    #[test]
    fn parse_simple_function() -> Result<()> {
        let func = parse_function("fn test() { return 12; }")?;
        assert_eq!(
            func,
            Statement::Function(
                "test".to_string(),
                vec![],
                Type::Void(),
                Box::new(Statement::Block(
                    vec![Statement::Return(
                        Expression::Literal(12, Type::UInt8(), 15..17),
                        12..17
                    )],
                    true,
                    0..20
                )),
                0..20
            )
        );
        Ok(())
    }

    #[test]
    fn parse_function_return_type() -> Result<()> {
        let func = parse_function("fn test() u8 { return 12; }")?;
        assert_eq!(
            func,
            Statement::Function(
                "test".to_string(),
                vec![],
                Type::UInt8(),
                Box::new(Statement::Block(
                    vec![Statement::Return(
                        Expression::Literal(12, Type::UInt8(), 15..17),
                        12..17
                    )],
                    true,
                    10..19
                )),
                0..20
            )
        );
        Ok(())
    }

    #[test]
    fn parse_function_call_expr() -> Result<()> {
        let call = parse_expressions("test()")?;
        assert_eq!(
            call[0],
            Expression::FunctionCall("test".to_string(), vec![], 0..6)
        );
        Ok(())
    }

    #[test]
    fn parse_string_constant_expr() -> Result<()> {
        let string = parse_expressions("\"test\"")?;
        assert_eq!(
            string[0],
            Expression::StringLiteral("test".to_string(), 0..8)
        );
        Ok(())
    }

    #[test]
    fn parse_string_constant_escaped_expr() -> Result<()> {
        let string = parse_expressions("\"test\\t\\'test\\'\\n\"")?;
        assert_eq!(
            string[0],
            Expression::StringLiteral("test\t\\'test\\'\n".to_string(), 0..20)
        );
        Ok(())
    }

    #[test]
    fn parse_function_call_stmt() -> Result<()> {
        let call = parse_statements("test();")?;
        assert_eq!(
            call[0],
            Statement::Expression(
                Expression::FunctionCall("test".to_string(), vec![], 0..7),
                0..7
            )
        );
        Ok(())
    }

    #[test]
    fn parse_function_call_stmt_with_arg() -> Result<()> {
        let call = parse_statements("test(x);")?;
        assert_eq!(
            call[0],
            Statement::Expression(
                Expression::FunctionCall(
                    "test".to_string(),
                    vec![Expression::VariableRef("x".to_string(), 5..6)],
                    0..8,
                ),
                0..8
            )
        );
        Ok(())
    }
}
