use std::collections::HashMap;
use std::str::FromStr;

use crate::{
    ast::{BinaryOperatorType, Expression, Statement, UnaryOperatorType},
    tokenizer::{Token, TokenType},
};
use dodo_core::{Error, Result, Type};

type PrefixParseFn<'a, C> = fn(&mut Parser<'a, C>) -> Result<Expression<C>>;
type InfixParseFn<'a, C> =
    fn(&mut Parser<'a, C>, left: Expression<C>, precedence: usize) -> Result<Expression<C>>;

pub struct Parser<'a, C> {
    tokens: &'a [Token],
    index: usize,
    prefix_fns: HashMap<TokenType, PrefixParseFn<'a, C>>,
    infix_fns: HashMap<TokenType, (InfixParseFn<'a, C>, usize)>,
}

impl<'a, C: FromStr> Parser<'a, C> {
    pub fn new(tokens: &'a [Token]) -> Self {
        let mut prefix_fns: HashMap<_, PrefixParseFn<'a, C>> = HashMap::new();
        prefix_fns.insert(TokenType::Identifier, Self::parse_identifier);
        prefix_fns.insert(TokenType::Minus, Self::parse_prefix_expression);
        prefix_fns.insert(TokenType::IntegerLiteral, Self::parse_constant);

        let mut infix_fns: HashMap<_, (InfixParseFn<'a, C>, usize)> = HashMap::new();
        infix_fns.insert(TokenType::Plus, (Self::parse_binary_operator, 3));
        infix_fns.insert(TokenType::Minus, (Self::parse_binary_operator, 3));
        infix_fns.insert(TokenType::Asterix, (Self::parse_binary_operator, 4));
        infix_fns.insert(TokenType::Slash, (Self::parse_binary_operator, 4));

        Self {
            tokens,
            index: 0,
            prefix_fns,
            infix_fns,
        }
    }

    pub fn eof(&self) -> bool {
        self.index >= self.tokens.len()
    }

    fn peek(&self) -> Result<&'a Token> {
        if self.eof() {
            Err(Error::TokenStreamOutOfBounds())
        } else {
            Ok(&self.tokens[self.index])
        }
    }

    fn consume(&mut self) -> Result<&'a Token> {
        let token = self.peek()?;
        self.index += 1;
        Ok(token)
    }

    fn consume_assert(&mut self, token_type: TokenType) -> Result<&'a Token> {
        let token = self.consume()?;
        assert_eq!(token.token_type, token_type);
        Ok(token)
    }

    fn parse_type(&mut self) -> Result<Type> {
        let token = self.consume()?;
        match token.token_type {
            TokenType::UInt8 => Ok(Type::UInt8()),
            TokenType::UInt16 => Ok(Type::UInt16()),
            TokenType::UInt32 => Ok(Type::UInt32()),
            _ => unreachable!(),
        }
    }

    fn parse_identifier(&mut self) -> Result<Expression<C>> {
        let token = self.consume_assert(TokenType::Identifier)?;
        Ok(Expression::VariableRef(token.value.clone()))
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression<C>> {
        let token = self.consume()?;
        let unop_type = UnaryOperatorType::from_token_type(token.token_type);
        let expression = self.parse_expression(0)?;
        Ok(Expression::UnaryOperator(unop_type, Box::new(expression)))
    }

    fn parse_constant(&mut self) -> Result<Expression<C>> {
        let token = self.consume_assert(TokenType::IntegerLiteral)?;

        match token.value.parse::<C>() {
            Ok(value) => Ok(Expression::Constant(value, Type::UInt8())),
            Err(_) => Err(Error::ParserError(format!(
                "Failed to parse {} to int",
                token.value
            ))),
        }
    }

    fn parse_binary_operator(
        &mut self,
        left: Expression<C>,
        precedence: usize,
    ) -> Result<Expression<C>> {
        let operator = self.consume()?;

        let op_type = BinaryOperatorType::from_token_type(operator.token_type);
        let right = self.parse_expression(precedence)?;
        Ok(Expression::BinaryOperator(
            op_type,
            Box::new(left),
            Box::new(right),
        ))
    }

    pub fn parse_expression(&mut self, precedence: usize) -> Result<Expression<C>> {
        let exit_tokens = vec![TokenType::SemiColon, TokenType::RightParen];

        let token_type = self.peek()?.token_type;

        let mut left = match self.prefix_fns.get(&token_type) {
            Some(func) => func(self),
            None => Err(Error::ParserError(format!(
                "Did not expect token: {:?} while parsing prefix expression",
                token_type
            ))),
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
                None => Err(Error::ParserError(format!(
                    "Did not expect token: {:?} while parsing infix expression",
                    token_type
                ))),
            }?;
        }

        Ok(left)
    }

    fn parse_return_statement(&mut self) -> Result<Statement<C>> {
        let expr = self.parse_expression(0)?;
        self.consume_assert(TokenType::SemiColon)?;
        Ok(Statement::Return(expr))
    }

    pub fn parse_function(&mut self) -> Result<Statement<C>> {
        self.consume_assert(TokenType::Fn)?;
        let identifier = self.consume_assert(TokenType::Identifier)?;
        self.consume_assert(TokenType::LeftParen)?;
        self.consume_assert(TokenType::RightParen)?;

        let mut return_type = Type::Void();

        if self.peek()?.token_type == TokenType::Colon {
            self.consume_assert(TokenType::Colon)?;
            return_type = self.parse_type()?;
        }

        let body = self.parse_statement()?;
        Ok(Statement::Function(
            identifier.value.clone(),
            vec![],
            return_type,
            Box::new(body),
        ))
    }

    pub fn parse_statement(&mut self) -> Result<Statement<C>> {
        let token = self.consume()?;
        match token.token_type {
            TokenType::Return => self.parse_return_statement(),
            TokenType::LeftBrace => {
                let mut statements = vec![];
                while self.peek()?.token_type != TokenType::RightBrace {
                    statements.push(self.parse_statement()?);
                }

                self.consume_assert(TokenType::RightBrace)?;

                Ok(Statement::Block(statements))
            }
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ast::BinaryOperatorType, tokenizer::tokenize};

    fn parse_statements(input: &str) -> Result<Vec<Statement<u64>>> {
        let tokens = tokenize(input)?;

        let mut parser = Parser::new(&tokens);
        let mut result = vec![];
        while !parser.eof() {
            result.push(parser.parse_statement()?);
        }

        Ok(result)
    }

    fn parse_expressions(input: &str) -> Result<Vec<Expression<u64>>> {
        let tokens = tokenize(input)?;

        let mut parser = Parser::new(&tokens);
        let mut result = vec![];
        while !parser.eof() {
            result.push(parser.parse_expression(0)?);
        }

        Ok(result)
    }

    fn parse_function(input: &str) -> Result<Statement<u64>> {
        let tokens = tokenize(input)?;

        let mut parser = Parser::new(&tokens);
        parser.parse_function()
    }

    #[test]
    fn parse_constant() -> Result<()> {
        let exprs = parse_expressions("123")?;

        assert_eq!(exprs[0], Expression::Constant(123, Type::UInt8()));

        Ok(())
    }

    #[test]
    fn parse_unary_op() -> Result<()> {
        let exprs = parse_expressions("-123")?;

        assert_eq!(
            exprs[0],
            Expression::UnaryOperator(
                UnaryOperatorType::Negate,
                Box::new(Expression::Constant(123, Type::UInt8()))
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
                Box::new(Expression::Constant(456, Type::UInt8())),
                Box::new(Expression::Constant(123, Type::UInt8()))
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
                Box::new(Expression::Constant(456, Type::UInt8())),
                Box::new(Expression::BinaryOperator(
                    BinaryOperatorType::Multiply,
                    Box::new(Expression::Constant(123, Type::UInt8())),
                    Box::new(Expression::Constant(789, Type::UInt8()))
                ))
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
                    Box::new(Expression::Constant(456, Type::UInt8())),
                    Box::new(Expression::Constant(123, Type::UInt8()))
                )),
                Box::new(Expression::Constant(789, Type::UInt8())),
            )
        );

        Ok(())
    }

    #[test]
    fn parse_return_statement() -> Result<()> {
        let stmts = parse_statements("return 456 - 123;")?;

        assert_eq!(
            stmts[0],
            Statement::Return(Expression::BinaryOperator(
                BinaryOperatorType::Subtract,
                Box::new(Expression::Constant(456, Type::UInt8())),
                Box::new(Expression::Constant(123, Type::UInt8()))
            ))
        );

        Ok(())
    }

    #[test]
    fn parse_empty_block() -> Result<()> {
        let stmts = parse_statements("{}")?;
        assert_eq!(stmts.len(), 1);
        assert_eq!(stmts[0], Statement::Block(vec![]));
        Ok(())
    }

    #[test]
    fn parse_nested_block() -> Result<()> {
        let stmts = parse_statements("{{{}}}")?;
        assert_eq!(stmts.len(), 1);
        assert_eq!(
            stmts[0],
            Statement::Block(vec![Statement::Block(vec![Statement::Block(vec![])])])
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
                Box::new(Statement::Block(vec![Statement::Return(Expression::Constant(
                    12,
                    Type::UInt8()
                ))]))
            )
        );
        Ok(())
    }

    #[test]
    fn parse_function_return_type() -> Result<()> {
        let func = parse_function("fn test(): u8 { return 12; }")?;
        assert_eq!(
            func,
            Statement::Function(
                "test".to_string(),
                vec![],
                Type::UInt8(),
                Box::new(Statement::Block(vec![Statement::Return(Expression::Constant(
                    12,
                    Type::UInt8()
                ))]))
            )
        );
        Ok(())
    }
}
