use crate::error::{Error, ErrorType, Result};
use logos::Logos;
use std::ops::Range;

#[derive(Logos, Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum TokenType {
    #[regex("[ \t\n\r]+")]
    Whitespace,
    #[regex("//[^\n]*")]
    Comment,

    #[token("struct")]
    Struct,
    #[token("extern")]
    Extern,
    #[token("return")]
    Return,
    #[token("fn")]
    Fn,
    #[token("const")]
    Const,
    #[token("let")]
    Let,
    #[token("while")]
    While,
    #[token("if")]
    If,
    #[token("else")]
    Else,

    #[token("true")]
    True,
    #[token("false")]
    False,

    #[regex("\"[^\"]*\"")]
    StringLiteral,
    #[regex("\'[^\']\'")]
    CharLiteral,
    #[regex("[0-9]+")]
    IntegerLiteral,
    #[regex("0[bB][0-1]+")]
    BinaryIntegerLiteral,
    #[regex("0[oO][0-8]+")]
    OctalIntegerLiteral,
    #[regex("0[xX][0-9a-fA-F]+")]
    HexIntegerLiteral,

    #[regex("[a-zA-Z][_0-9a-zA-Z]*")]
    Identifier,

    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Asterisk,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("<<")]
    DoubleLessThan,
    #[token(">>")]
    DoubleGreaterThan,

    #[token("==")]
    DoubleEqual,
    #[token("!=")]
    NotEqual,
    #[token("<")]
    LessThan,
    #[token("<=")]
    LessThanEqual,
    #[token(">")]
    GreaterThan,
    #[token(">=")]
    GreaterThanEqual,
    #[token("=")]
    Equals,

    #[token("||")]
    DoubleVerticalBar,
    #[token("&&")]
    DoubleAmpersand,

    #[token("&")]
    Ampersand,
    #[token("@")]
    At,

    #[token(":")]
    Colon,
    #[token(";")]
    SemiColon,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,

    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,

    #[token("{")]
    LeftBrace,
    #[token("}")]
    RightBrace,

    #[token("[")]
    LeftSquareParen,
    #[token("]")]
    RightSquareParen,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SourceRange {
    pub start: usize,
    pub end: usize,
}

impl SourceRange {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn empty() -> Self {
        Self { start: 0, end: 0 }
    }

    pub fn expand(&self, other: &Self) -> Self {
        SourceRange {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

impl From<Range<usize>> for SourceRange {
    fn from(range: Range<usize>) -> Self {
        Self {
            start: range.start,
            end: range.end,
        }
    }
}

impl From<SourceRange> for Range<usize> {
    fn from(range: SourceRange) -> Self {
        range.start..range.end
    }
}

#[derive(Debug)]
pub struct Token<'a> {
    pub token_type: TokenType,
    pub value: &'a str,
    pub range: SourceRange,
}

impl<'a> Token<'a> {
    pub fn new(token_type: TokenType, value: &'a str, range: SourceRange) -> Self {
        Self {
            token_type,
            value,
            range,
        }
    }
}

impl<'a> PartialEq for Token<'a> {
    fn eq(&self, other: &Self) -> bool {
        // NOTE: comparing the actual value might also be necessary
        self.token_type == other.token_type && self.range == other.range
    }
}

impl<'a> PartialEq<TokenType> for &Token<'a> {
    fn eq(&self, other: &TokenType) -> bool {
        self.token_type == *other
    }
}

pub fn lex(input: &str) -> Result<Vec<Token>> {
    let mut lex = TokenType::lexer(input);

    let mut tokens = vec![];

    while let Some(token_type) = lex.next() {
        match token_type {
            Err(_) => {
                return Err(Error::new_with_range(
                    ErrorType::Lexer,
                    "Unknown character".to_string(),
                    lex.span().into(),
                ))
            }
            Ok(TokenType::Whitespace) | Ok(TokenType::Comment) => continue,
            Ok(token_type) => tokens.push(Token::new(token_type, lex.slice(), lex.span().into())),
        }
    }

    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::TokenType::*;
    use super::*;

    fn get_tokens(input: &str) -> Vec<Token> {
        let tokens = lex(input);
        assert!(tokens.is_ok());
        tokens.unwrap()
    }

    #[test]
    fn tokenizer_empty() {
        let tokens = get_tokens("");
        assert_eq!(tokens.len(), 0);
    }

    #[test]
    fn tokenizer_string() {
        let tokens = get_tokens("\"test\" \"test 123 ' 12312\"");

        assert_eq!(tokens[0].token_type, StringLiteral);
        assert_eq!(tokens[1].token_type, StringLiteral);
    }

    #[test]
    fn tokenizer_keywords() {
        let tokens = get_tokens("return");

        assert_eq!(tokens[0].token_type, Return);
    }

    #[test]
    fn tokenizer_integer_literal() {
        let tokens = get_tokens("12 0 439394474 123");

        assert_eq!(tokens[0], Token::new(IntegerLiteral, "12", (0..2).into()));
        assert_eq!(tokens[1], Token::new(IntegerLiteral, "0", (3..4).into()));
        assert_eq!(
            tokens[2],
            Token::new(IntegerLiteral, "439394474", (5..14).into())
        );
        assert_eq!(
            tokens[3],
            Token::new(IntegerLiteral, "123", (15..18).into())
        );
    }

    #[test]
    fn tokenizer_binary_operators() {
        let tokens = get_tokens("+-*/");

        assert_eq!(tokens[0].token_type, Plus);
        assert_eq!(tokens[1].token_type, Minus);
        assert_eq!(tokens[2].token_type, Asterisk);
        assert_eq!(tokens[3].token_type, Slash);
    }

    #[test]
    fn tokenizer_test_comparison_operators() {
        let tokens = get_tokens("== != < <= > >=");

        assert_eq!(tokens[0].token_type, DoubleEqual);
        assert_eq!(tokens[1].token_type, NotEqual);
        assert_eq!(tokens[2].token_type, LessThan);
        assert_eq!(tokens[3].token_type, LessThanEqual);
        assert_eq!(tokens[4].token_type, GreaterThan);
        assert_eq!(tokens[5].token_type, GreaterThanEqual);
    }

    #[test]
    fn tokenizer_identifier() {
        let tokens = get_tokens("test test_with_underscore");

        assert_eq!(tokens[0], Token::new(Identifier, "test", (0..4).into()));
        assert_eq!(
            tokens[1],
            Token::new(Identifier, "test_with_underscore", (5..25).into())
        );
    }

    #[test]
    fn tokenizer_identifier_error() {
        let tokens = lex("_identifier");

        assert!(tokens.is_err());
    }

    #[test]
    fn tokenizer_test_error() {
        let tokens = lex("return #;");

        assert!(tokens.is_err());
    }
}
