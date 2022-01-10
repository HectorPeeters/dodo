use crate::error::{Error, Result};
use regex::Regex;
use std::ops::Range;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum TokenType {
    Whitespace,

    Return,
    Fn,
    Let,

    UInt8,
    UInt16,
    UInt32,
    Bool,

    Identifier,

    IntegerLiteral,

    Plus,
    Minus,
    Asterix,
    Slash,
    DoubleEqual,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,

    Equals,

    Colon,
    SemiColon,

    LeftParen,
    RightParen,

    LeftBrace,
    RightBrace,
}

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub value: String,
    pub span: Range<usize>,
}

impl Token {
    pub fn new(token_type: TokenType, value: &str) -> Self {
        Self {
            token_type,
            value: value.to_string(),
            span: 0..0,
        }
    }

    pub fn with_type(token_type: TokenType) -> Self {
        Self {
            token_type,
            value: String::default(),
            span: 0..0,
        }
    }

    pub fn new_with_span(token_type: TokenType, value: &str, span: Range<usize>) -> Self {
        Self {
            token_type,
            value: value.to_string(),
            span,
        }
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.token_type == other.token_type && self.value == other.value
    }
}

impl PartialEq<TokenType> for &Token {
    fn eq(&self, other: &TokenType) -> bool {
        self.token_type == *other
    }
}

pub struct Lexer<'a> {
    rules: Vec<(Regex, TokenType)>,
    input: &'a str,
    pointer: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        use TokenType::*;

        let rules = vec![
            (r"[ \t\n\f]+", Whitespace),
            (r"return", Return),
            (r"let", Let),
            (r"fn", Fn),
            (r"u8", UInt8),
            (r"u16", UInt16),
            (r"u32", UInt32),
            (r"bool", Bool),
            (r"[a-zA-Z][_0-9a-zA-Z]*", Identifier),
            (r"[0-9]+", IntegerLiteral),
            (r"\+", Plus),
            (r"-", Minus),
            (r"\*", Asterix),
            (r"/", Slash),
            (r"==", DoubleEqual),
            (r"!=", NotEqual),
            (r"<=", LessThanEqual),
            (r"<", LessThan),
            (r">=", GreaterThanEqual),
            (r">", GreaterThan),
            (r"=", Equals),
            (r":", Colon),
            (r";", SemiColon),
            (r"\(", LeftParen),
            (r"\)", RightParen),
            (r"\{", LeftBrace),
            (r"\}", RightBrace),
        ];

        let rules = rules
            .into_iter()
            .map(|x| (Regex::new(x.0).unwrap(), x.1))
            .collect();

        Self {
            rules,
            input,
            pointer: 0,
        }
    }

    pub fn get_tokens(&mut self) -> Result<Vec<Token>> {
        let mut result = vec![];

        loop {
            if self.pointer >= self.input.len() {
                break;
            }

            let mut matches = vec![];

            for rule in &self.rules {
                let (regex, token_type) = rule;

                if let Some(x) = regex.find(&self.input[self.pointer..]) {
                    if x.start() == 0 {
                        matches.push(Token::new_with_span(*token_type, x.as_str(), x.range()));
                    }
                }
            }

            if matches.is_empty() {
                return Err(Error::LexerError("Unable to tokenize".to_string()));
            }

            matches.sort_by(|a, b| (b.span.end - b.span.start).cmp(&(a.span.end - a.span.start)));

            let best_match = matches.remove(0);
            self.pointer += best_match.span.end;

            result.push(best_match);
        }

        Ok(result
            .into_iter()
            .filter(|x| x.token_type != TokenType::Whitespace)
            .collect())
    }
}

pub fn tokenize(input: &str) -> Result<Vec<Token>> {
    Lexer::new(input).get_tokens()
}

#[cfg(test)]
mod tests {
    use super::TokenType::*;
    use super::*;

    fn get_tokens(input: &str) -> Vec<Token> {
        let tokens = tokenize(input);
        assert!(tokens.is_ok());
        tokens.unwrap()
    }

    #[test]
    fn tokenizer_empty() {
        let tokens = get_tokens("");
        assert_eq!(tokens.len(), 0);
    }

    #[test]
    fn tokenizer_types() {
        let tokens = get_tokens("u8 u16 u32 bool");

        assert_eq!(tokens[0].token_type, UInt8);
        assert_eq!(tokens[1].token_type, UInt16);
        assert_eq!(tokens[2].token_type, UInt32);
        assert_eq!(tokens[3].token_type, Bool);
    }

    #[test]
    fn tokenizer_keywords() {
        let tokens = get_tokens("return");

        assert_eq!(tokens[0].token_type, Return);
    }

    #[test]
    fn tokenizer_integer_literal() {
        let tokens = get_tokens("12 0 439394474 123");

        assert_eq!(tokens[0], Token::new(IntegerLiteral, "12"));
        assert_eq!(tokens[1], Token::new(IntegerLiteral, "0"));
        assert_eq!(tokens[2], Token::new(IntegerLiteral, "439394474"));
        assert_eq!(tokens[3], Token::new(IntegerLiteral, "123"));
    }

    #[test]
    fn tokenizer_binary_operators() {
        let tokens = get_tokens("+-*/");

        assert_eq!(tokens[0].token_type, Plus);
        assert_eq!(tokens[1].token_type, Minus);
        assert_eq!(tokens[2].token_type, Asterix);
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

        assert_eq!(tokens[0], Token::new(Identifier, "test"));
        assert_eq!(tokens[1], Token::new(Identifier, "test_with_underscore"));
    }

    #[test]
    fn tokenizer_identifier_error() {
        let tokens = tokenize("_identifier");

        assert!(tokens.is_err());
    }

    #[test]
    fn tokenizer_test_error() {
        let tokens = tokenize("return &;");

        assert!(tokens.is_err());
    }
}
