use dodo_core::Type;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, multispace0, one_of},
    combinator::{map, recognize, value},
    error::ParseError,
    multi::{many0, many1},
    sequence::{delimited, pair, terminated, tuple},
    IResult,
};

use crate::ast::{
    BinaryOperatorExpression, BinaryOperatorType, BlockStatement, ConstantExpression,
    DeclarationStatement, Expression, ReturnStatement, Statement, UnaryOperatorExpression,
    UnaryOperatorType,
};

macro_rules! ws {
    ($x: expr) => {
        delimited(multispace0, $x, multispace0)
    };
}

fn parse_type<'a, E>(input: &'a str) -> IResult<&'a str, Type, E>
where
    E: ParseError<&'a str>,
{
    alt((
        value(Type::Nil(), tag("nil")),
        value(Type::UInt8(), tag("u8")),
        value(Type::UInt16(), tag("u16")),
        value(Type::UInt32(), tag("u32")),
    ))(input)
}

fn parse_unary_operator_type<'a, E>(input: &'a str) -> IResult<&'a str, UnaryOperatorType, E>
where
    E: ParseError<&'a str>,
{
    alt((
        value(UnaryOperatorType::Negate, tag("-")),
        value(UnaryOperatorType::Ref, tag("&")),
        value(UnaryOperatorType::Deref, tag("*")),
    ))(input)
}

fn parse_binary_operator_type<'a, E>(input: &'a str) -> IResult<&'a str, BinaryOperatorType, E>
where
    E: ParseError<&'a str>,
{
    alt((
        value(BinaryOperatorType::Add, tag("+")),
        value(BinaryOperatorType::Subtract, tag("-")),
        value(BinaryOperatorType::Multiply, tag("*")),
        value(BinaryOperatorType::Divide, tag("/")),
    ))(input)
}

fn parse_identifier<'a, E>(input: &'a str) -> IResult<&'a str, String, E>
where
    E: ParseError<&'a str>,
{
    map(
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        )),
        |x: &str| x.to_string(),
    )(input)
}

pub fn parse_statement<'a, E>(input: &'a str) -> IResult<&'a str, Statement, E>
where
    E: ParseError<&'a str>,
{
    alt((
        map(parse_block, Statement::Block),
        map(parse_return, Statement::Return),
        map(parse_declaration, Statement::Declaration),
    ))(input)
}

fn parse_block<'a, E>(input: &'a str) -> IResult<&'a str, BlockStatement, E>
where
    E: ParseError<&'a str>,
{
    map(
        delimited(char('{'), ws!(many0(parse_statement)), char('}')),
        |x| BlockStatement { children: x },
    )(input)
}

fn parse_return<'a, E>(input: &'a str) -> IResult<&'a str, ReturnStatement, E>
where
    E: ParseError<&'a str>,
{
    map(
        delimited(tag("return"), ws!(parse_expression), char(';')),
        |x| ReturnStatement { value: x },
    )(input)
}

fn parse_declaration<'a, E>(input: &'a str) -> IResult<&'a str, DeclarationStatement, E>
where
    E: ParseError<&'a str>,
{
    map(
        tuple((
            tag("let"),
            ws!(parse_identifier),
            ws!(char(':')),
            parse_type,
            ws!(char(';')),
        )),
        |x| DeclarationStatement {
            name: x.1,
            variable_type: x.3,
        },
    )(input)
}

fn parse_expression<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
where
    E: ParseError<&'a str>,
{
    alt((
        map(parse_unary_operator, Expression::UnaryOperator),
        map(parse_constant, Expression::Constant),
        map(parse_binary_operator, Expression::BinaryOperator),
    ))(input)
}

fn parse_constant<'a, E>(input: &'a str) -> IResult<&'a str, ConstantExpression, E>
where
    E: ParseError<&'a str>,
{
    map(
        recognize(many1(terminated(one_of("0123456789"), many0(char('_'))))),
        |x: &str| {
            ConstantExpression::new(
                x.parse()
                    .unwrap_or_else(|_| panic!("Failed to parse int {}", x)),
                Type::UInt32(),
            )
        },
    )(input)
}

fn parse_unary_operator<'a, E>(input: &'a str) -> IResult<&'a str, UnaryOperatorExpression, E>
where
    E: ParseError<&'a str>,
{
    map(
        pair(parse_unary_operator_type, ws!(parse_expression)),
        |x| UnaryOperatorExpression::new(x.0, Box::new(x.1)),
    )(input)
}

fn parse_binary_operator<'a, E>(input: &'a str) -> IResult<&'a str, BinaryOperatorExpression, E>
where
    E: ParseError<&'a str>,
{
    map(
        tuple((
            parse_expression,
            ws!(parse_binary_operator_type),
            parse_expression,
        )),
        |x| BinaryOperatorExpression::new(x.1, Box::new(x.0), Box::new(x.2)),
    )(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use Type::*;

    #[test]
    fn parser_type() {
        assert_eq!(parse_type::<()>("nil"), Ok(("", Nil())));
        assert_eq!(parse_type::<()>("u8"), Ok(("", UInt8())));
        assert_eq!(parse_type::<()>("u16"), Ok(("", UInt16())));
        assert_eq!(parse_type::<()>("u32"), Ok(("", UInt32())));

        assert_eq!(parse_type::<()>("u8 tust"), Ok((" tust", UInt8())));
    }

    #[test]
    fn parser_unop() {
        assert_eq!(
            parse_unary_operator_type::<()>("-"),
            Ok(("", UnaryOperatorType::Negate))
        );
        assert_eq!(
            parse_unary_operator_type::<()>("&"),
            Ok(("", UnaryOperatorType::Ref))
        );
        assert_eq!(
            parse_unary_operator_type::<()>("*"),
            Ok(("", UnaryOperatorType::Deref))
        );
    }

    #[test]
    fn parser_binop() {
        assert_eq!(
            parse_binary_operator_type::<()>("+"),
            Ok(("", BinaryOperatorType::Add))
        );
        assert_eq!(
            parse_binary_operator_type::<()>("-"),
            Ok(("", BinaryOperatorType::Subtract))
        );
        assert_eq!(
            parse_binary_operator_type::<()>("*"),
            Ok(("", BinaryOperatorType::Multiply))
        );
        assert_eq!(
            parse_binary_operator_type::<()>("/"),
            Ok(("", BinaryOperatorType::Divide))
        );
    }

    #[test]
    fn parser_identifier() {
        assert_eq!(parse_identifier::<()>("test"), Ok(("", "test".to_string())));
        assert_eq!(parse_identifier::<()>("t"), Ok(("", "t".to_string())));
    }

    #[test]
    fn parser_block() {
        assert_eq!(
            parse_block::<()>("{}"),
            Ok(("", BlockStatement::new(vec![])))
        );
        assert_eq!(
            parse_block::<()>("{ }"),
            Ok(("", BlockStatement::new(vec![])))
        );
        assert_eq!(
            parse_block::<()>("{{}}"),
            Ok((
                "",
                BlockStatement::new(vec![Statement::Block(BlockStatement::new(vec![]))])
            ))
        );
        assert_eq!(
            parse_block::<()>("{ { }  }"),
            Ok((
                "",
                BlockStatement::new(vec![Statement::Block(BlockStatement::new(vec![]))])
            ))
        );
    }

    #[test]
    fn parser_declaration() {
        assert_eq!(
            parse_declaration::<()>("let x: u32;"),
            Ok(("", DeclarationStatement::new("x".to_string(), UInt32())))
        );
        assert_eq!(
            parse_declaration::<()>("let x:u32;"),
            Ok(("", DeclarationStatement::new("x".to_string(), UInt32())))
        );
        assert_eq!(
            parse_declaration::<()>("let x : u32;"),
            Ok(("", DeclarationStatement::new("x".to_string(), UInt32())))
        );
    }

    #[test]
    fn parser_statement() {
        assert_eq!(
            parse_statement::<()>("{ let x: u32; }"),
            Ok((
                "",
                Statement::Block(BlockStatement::new(vec![Statement::Declaration(
                    DeclarationStatement::new("x".to_string(), UInt32())
                )]))
            ))
        );
        assert_eq!(
            parse_statement::<()>("{ let x: u32;\nlet y: u8; }"),
            Ok((
                "",
                Statement::Block(BlockStatement::new(vec![
                    Statement::Declaration(DeclarationStatement::new("x".to_string(), UInt32())),
                    Statement::Declaration(DeclarationStatement::new("y".to_string(), UInt8()))
                ]))
            ))
        );
    }

    #[test]
    fn parser_constant() {
        assert_eq!(
            parse_constant::<()>("12345"),
            Ok(("", ConstantExpression::new(12345, UInt32())))
        );
        assert_eq!(
            parse_constant::<()>("0"),
            Ok(("", ConstantExpression::new(0, UInt32())))
        );
    }

    #[test]
    fn parser_unary_operator() {
        assert_eq!(
            parse_unary_operator::<()>("-12"),
            Ok((
                "",
                UnaryOperatorExpression::new(
                    UnaryOperatorType::Negate,
                    Box::new(Expression::Constant(ConstantExpression::new(12, UInt32())))
                )
            ))
        );
        assert_eq!(
            parse_unary_operator::<()>("- 12"),
            Ok((
                "",
                UnaryOperatorExpression::new(
                    UnaryOperatorType::Negate,
                    Box::new(Expression::Constant(ConstantExpression::new(12, UInt32())))
                )
            ))
        );
        assert_eq!(
            parse_unary_operator::<()>("--12"),
            Ok((
                "",
                UnaryOperatorExpression::new(
                    UnaryOperatorType::Negate,
                    Box::new(Expression::UnaryOperator(UnaryOperatorExpression::new(
                        UnaryOperatorType::Negate,
                        Box::new(Expression::Constant(ConstantExpression::new(12, UInt32())))
                    )))
                )
            ))
        );
        assert_eq!(
            parse_unary_operator::<()>("&12"),
            Ok((
                "",
                UnaryOperatorExpression::new(
                    UnaryOperatorType::Ref,
                    Box::new(Expression::Constant(ConstantExpression::new(12, UInt32())))
                )
            ))
        );
        assert_eq!(
            parse_unary_operator::<()>("*12"),
            Ok((
                "",
                UnaryOperatorExpression::new(
                    UnaryOperatorType::Deref,
                    Box::new(Expression::Constant(ConstantExpression::new(12, UInt32())))
                )
            ))
        );
    }

    #[test]
    fn parser_binary_operator() {
        assert_eq!(
            parse_binary_operator::<()>("1+12"),
            Ok((
                "",
                BinaryOperatorExpression::new(
                    BinaryOperatorType::Add,
                    Box::new(Expression::Constant(ConstantExpression::new(1, UInt32()))),
                    Box::new(Expression::Constant(ConstantExpression::new(12, UInt32())))
                )
            ))
        );

        assert_eq!(
            parse_binary_operator::<()>("1 -12"),
            Ok((
                "",
                BinaryOperatorExpression::new(
                    BinaryOperatorType::Subtract,
                    Box::new(Expression::Constant(ConstantExpression::new(1, UInt32()))),
                    Box::new(Expression::Constant(ConstantExpression::new(12, UInt32())))
                )
            ))
        );

        assert_eq!(
            parse_binary_operator::<()>("1* 12"),
            Ok((
                "",
                BinaryOperatorExpression::new(
                    BinaryOperatorType::Multiply,
                    Box::new(Expression::Constant(ConstantExpression::new(1, UInt32()))),
                    Box::new(Expression::Constant(ConstantExpression::new(12, UInt32())))
                )
            ))
        );

        assert_eq!(
            parse_binary_operator::<()>("1 / 12"),
            Ok((
                "",
                BinaryOperatorExpression::new(
                    BinaryOperatorType::Divide,
                    Box::new(Expression::Constant(ConstantExpression::new(1, UInt32()))),
                    Box::new(Expression::Constant(ConstantExpression::new(12, UInt32())))
                )
            ))
        );
    }
}
