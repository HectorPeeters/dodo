use crate::error::{Error, ErrorType, Result};
use regex::Regex;
use std::ops::Range;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum TokenType {
    Whitespace,
    Comment,

    Return,
    Fn,
    Let,
    While,
    If,

    UInt8,
    UInt16,
    UInt32,
    UInt64,
    Bool,
    StringLiteral,
    IntegerLiteral,

    Identifier,

    Plus,
    Minus,
    Asterisk,
    Slash,
    DoubleEqual,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,

    Equals,

    Ampersand,

    Colon,
    SemiColon,
    Comma,

    LeftParen,
    RightParen,

    LeftBrace,
    RightBrace,
}

pub type SourceRange = Range<usize>;

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

    pub fn offset(&mut self, offset: usize) {
        self.range = self.range.start + offset..self.range.end + offset;
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

pub struct Lexer<'a> {
    rules: Vec<(Regex, TokenType)>,
    input: &'a str,
    pointer: usize,
    input_file: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str, input_file: &'a str) -> Self {
        use TokenType::*;

        let rules = vec![
            (r"[ \t\n\f]+", Whitespace),
            (r"//[^\n]*", Comment),
            ("\"[^\"]*\"", StringLiteral),
            (r"return", Return),
            (r"let", Let),
            (r"while", While),
            (r"if", If),
            (r"fn", Fn),
            (r"u8", UInt8),
            (r"u16", UInt16),
            (r"u32", UInt32),
            (r"u64", UInt64),
            (r"bool", Bool),
            (r"[a-zA-Z][_0-9a-zA-Z]*", Identifier),
            (r"[0-9]+", IntegerLiteral),
            (r"\+", Plus),
            (r"-", Minus),
            (r"\*", Asterisk),
            (r"/", Slash),
            (r"==", DoubleEqual),
            (r"!=", NotEqual),
            (r"<=", LessThanEqual),
            (r"<", LessThan),
            (r">=", GreaterThanEqual),
            (r">", GreaterThan),
            (r"=", Equals),
            (r"&", Ampersand),
            (r":", Colon),
            (r";", SemiColon),
            (r",", Comma),
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
            input_file,
        }
    }

    pub fn get_tokens(&mut self) -> Result<Vec<Token<'a>>> {
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
                        matches.push(Token::new(
                            *token_type,
                            x.as_str(),
                            x.range().start..x.range().end,
                        ));
                    }
                }
            }

            if matches.is_empty() {
                return Err(Error::new(
                    ErrorType::Lexer,
                    "Unexpected character".to_string(),
                    self.pointer..self.pointer + 1,
                    self.input_file.to_string(),
                ));
            }

            matches
                .sort_by(|a, b| (b.range.end - b.range.start).cmp(&(a.range.end - a.range.start)));

            let mut best_match = matches.remove(0);
            let match_size = best_match.range.end;
            best_match.offset(self.pointer);
            self.pointer += match_size;

            result.push(best_match);
        }

        Ok(result
            .into_iter()
            .filter(|x| x.token_type != TokenType::Whitespace)
            .filter(|x| x.token_type != TokenType::Comment)
            .collect())
    }
}

pub fn tokenize<'a>(input: &'a str, file: &'a str) -> Result<Vec<Token<'a>>> {
    let mut lexer = Lexer::new(input, file);
    lexer.get_tokens()
}

#[cfg(test)]
mod tests {
    use super::TokenType::*;
    use super::*;

    fn get_tokens(input: &str) -> Vec<Token> {
        let tokens = tokenize(input, "test.dodo");
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

        assert_eq!(tokens[0], Token::new(IntegerLiteral, "12", 0..2));
        assert_eq!(tokens[1], Token::new(IntegerLiteral, "0", 3..4));
        assert_eq!(tokens[2], Token::new(IntegerLiteral, "439394474", 5..14));
        assert_eq!(tokens[3], Token::new(IntegerLiteral, "123", 15..18));
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

        assert_eq!(tokens[0], Token::new(Identifier, "test", 0..4));
        assert_eq!(
            tokens[1],
            Token::new(Identifier, "test_with_underscore", 5..25)
        );
    }

    #[test]
    fn tokenizer_identifier_error() {
        let tokens = tokenize("_identifier", "test.dodo");

        assert!(tokens.is_err());
    }

    #[test]
    fn tokenizer_test_error() {
        let tokens = tokenize("return #;", "test.dodo");

        assert!(tokens.is_err());
    }
}
