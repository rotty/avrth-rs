use combine::error::{ParseError, StreamError};
use combine::parser::byte::{alpha_num, byte, bytes, digit, hex_digit, letter};
use combine::parser::range::{range, recognize};
use combine::parser::repeat::skip_until;
use combine::stream::{RangeStream, Stream, StreamErrorFor};
use combine::{
    any, attempt, between, choice, many, none_of, optional, satisfy, skip_many, skip_many1, Parser,
};

use std::fmt;
use std::str;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Token<'a> {
    Directive(&'a str),
    Ident(&'a str),
    Str(String),
    Int(i64),
    Char(u8),
    Eol,
    LParen,
    RParen,
    Colon,
    Comma,
    Assign,
    At,
    Plus,
    Minus,
    Star,
    Slash,
    Mod,
    ShiftLeft,
    ShiftRight,
    Equals,
    NotEquals,
    Less,
    Greater,
    LessOrEqual,
    GreaterOrEqual,
    BitNot,
    BitAnd,
    BitOr,
    BitXor,
    LogicalAnd,
    LogicalOr,
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::Token::*;
        match self {
            Directive(s) => write!(f, ".{}", s),
            Ident(s) => write!(f, "{}", s),
            Str(s) => write!(f, r#""{}""#, s),
            Int(n) => write!(f, "{}", n),
            Char(c) => write!(f, "'{}'", c),
            Eol => f.write_str("EOL"),
            LParen => f.write_str("("),
            RParen => f.write_str(")"),
            Colon => f.write_str(":"),
            Comma => f.write_str(","),
            Assign => f.write_str("="),
            At => f.write_str("@"),
            Plus => f.write_str("+"),
            Minus => f.write_str("-"),
            Star => f.write_str("*"),
            Slash => f.write_str("/"),
            Mod => f.write_str("%"),
            ShiftLeft => f.write_str("<<"),
            ShiftRight => f.write_str(">>"),
            Equals => f.write_str("=="),
            NotEquals => f.write_str("!="),
            Less => f.write_str("<"),
            Greater => f.write_str(">"),
            LessOrEqual => f.write_str("<="),
            GreaterOrEqual => f.write_str(">="),
            BitNot => f.write_str("~"),
            BitAnd => f.write_str("&"),
            BitOr => f.write_str("|"),
            BitXor => f.write_str("^"),
            LogicalAnd => f.write_str("&&"),
            LogicalOr => f.write_str("||"),
        }
    }
}

fn is_hspace(c: u8) -> bool {
    c == b' ' || c == b'\t'
}

fn hspace<'a, I>() -> impl Parser<I, Output = ()>
where
    I: RangeStream<Token = u8, Range = &'a [u8]>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
{
    let comment = byte(b';').with(skip_until(satisfy(|c| c == b'\n' || c == b'\r')));
    skip_many(satisfy(is_hspace))
        .map(|_| ())
        .skip(optional(comment))
        .silent()
}

fn ident_str<'a, I>() -> impl Parser<I, Output = &'a str>
where
    I: RangeStream<Token = u8, Range = &'a [u8]>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
{
    recognize(skip_many1(letter().or(byte(b'_'))).with(skip_many(alpha_num().or(byte(b'_')))))
        .map(|ident| str::from_utf8(ident).unwrap())
}

fn directive_str<'a, I>() -> impl Parser<I, Output = &'a str>
where
    I: RangeStream<Token = u8, Range = &'a [u8]>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
{
    byte(b'.')
        .with(recognize(
            skip_many1(letter().or(byte(b'_'))).with(skip_many(alpha_num().or(byte(b'_')))),
        ))
        .map(|ident| str::from_utf8(ident).unwrap())
}

fn int_token<'a, I>() -> impl Parser<I, Output = Token<'a>>
where
    I: RangeStream<Token = u8, Range = &'a [u8]>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
{
    let decimal_digits = || recognize(skip_many1(digit())).map(|digits| (10, digits));
    let hex_digits = || {
        choice((
            byte(b'$').map(|_| ()),
            attempt(range(&b"0x"[..])).map(|_| ()),
        ))
        .with(recognize(skip_many1(hex_digit())).map(|digits| (16, digits)))
    };
    choice((hex_digits(), decimal_digits())).and_then(|(base, digits): (_, &[u8])| {
        let digits = str::from_utf8(&digits).unwrap();
        i64::from_str_radix(digits, base)
            .map(Token::Int)
            .map_err(StreamErrorFor::<I>::other)
    })
}

fn operator_token<'a, I>() -> impl Parser<I, Output = Token<'a>>
where
    I: RangeStream<Token = u8, Range = &'a [u8]>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
{
    choice((
        attempt(bytes(b"||")).map(|_| Token::LogicalOr),
        attempt(bytes(b"&&")).map(|_| Token::LogicalAnd),
        attempt(bytes(b"<<")).map(|_| Token::ShiftLeft),
        attempt(bytes(b">>")).map(|_| Token::ShiftRight),
        attempt(bytes(b"==")).map(|_| Token::Equals),
        attempt(bytes(b"!=")).map(|_| Token::NotEquals),
        attempt(bytes(b">=")).map(|_| Token::GreaterOrEqual),
        attempt(bytes(b"<=")).map(|_| Token::LessOrEqual),
        byte(b'<').map(|_| Token::Less),
        byte(b'>').map(|_| Token::Greater),
        byte(b'+').map(|_| Token::Plus),
        byte(b'-').map(|_| Token::Minus),
        byte(b'*').map(|_| Token::Star),
        byte(b'/').map(|_| Token::Slash),
        byte(b'%').map(|_| Token::Mod),
        byte(b'&').map(|_| Token::BitAnd),
        byte(b'|').map(|_| Token::BitOr),
        byte(b'~').map(|_| Token::BitNot),
        byte(b',').map(|_| Token::Comma),
        byte(b'=').map(|_| Token::Assign),
        byte(b':').map(|_| Token::Colon),
    ))
}

fn escaped_byte<I>(terminators: &'static [u8]) -> impl Parser<I, Output = u8>
where
    I: Stream<Token = u8>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
{
    choice((
        byte(b'\\').with(any().map(|escaped| match escaped {
            b'n' => b'\n',
            b't' => b'\t',
            b'r' => b'\r',
            _ => escaped,
        })),
        none_of(terminators.iter().cloned()),
    ))
}

fn char_literal<I>() -> impl Parser<I, Output = u8>
where
    I: Stream<Token = u8>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
{
    between(byte(b'\''), byte(b'\''), escaped_byte(&[b'\'']))
}

fn string_literal<I>() -> impl Parser<I, Output = String>
where
    I: Stream<Token = u8>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
{
    between(byte(b'"'), byte(b'"'), many(escaped_byte(&[b'"'])))
        .and_then(|bs| String::from_utf8(bs).map_err(StreamErrorFor::<I>::other))
}

pub fn tokens<'a, I>() -> impl Parser<I, Output = Vec<Token<'a>>>
where
    I: RangeStream<Token = u8, Range = &'a [u8]>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
{
    fn unit<T>(_: T) {}
    many(
        choice((
            byte(b'@').map(|_| Token::At),
            byte(b'(').map(|_| Token::LParen),
            byte(b')').map(|_| Token::RParen),
            ident_str().map(Token::Ident),
            directive_str().map(Token::Directive),
            int_token(),
            string_literal().map(Token::Str),
            char_literal().map(Token::Char),
            operator_token(),
            hspace()
                .with(choice((
                    attempt(bytes(b"\r\n")).map(unit),
                    byte(b'\r').map(unit),
                    byte(b'\n').map(unit),
                )))
                .map(|_| Token::Eol),
        ))
        .skip(hspace()),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use combine::EasyParser;

    fn parse_tokens(input: &str) -> Vec<Token> {
        let (parsed, remainder) = tokens()
            .easy_parse(input.as_bytes())
            .expect("unable to parse");
        assert_eq!(remainder, &b""[..]);
        parsed
    }

    #[test]
    fn test_hspace() {
        assert_eq!(
            hspace().easy_parse(&b""[..]).expect("parsing failed"),
            ((), "".as_bytes())
        );
        assert_eq!(
            hspace().easy_parse(&b"\n"[..]).expect("parsing failed"),
            ((), "\n".as_bytes())
        );
        assert_eq!(
            hspace().easy_parse(&b"\t  \n"[..]).expect("parsing failed"),
            ((), "\n".as_bytes())
        );
    }

    #[test]
    fn test_vspace() {
        assert_eq!(
            tokens().easy_parse(&b"\n"[..]).expect("parsing failed"),
            (vec![Token::Eol], "".as_bytes())
        );
        assert_eq!(
            tokens()
                .easy_parse(&b"\n; A comment with EOL\n"[..])
                .expect("parsing failed"),
            (vec![Token::Eol, Token::Eol], "".as_bytes())
        );
        assert_eq!(
            tokens()
                .easy_parse(&b"\n; A comment with EOL\n  "[..])
                .expect("parsing failed"),
            (vec![Token::Eol, Token::Eol], "".as_bytes())
        );
    }

    #[test]
    fn test_int() {
        assert_eq!(
            parse_tokens("1 2 3 42 0xFFEED $ABCD  "),
            vec![1, 2, 3, 42, 0xFFEED, 0xABCD]
                .into_iter()
                .map(Token::Int)
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_char() {
        assert_eq!(
            parse_tokens(r#"'2' '3' '\t' '\''  "#),
            vec![b'2', b'3', b'\t', b'\'']
                .into_iter()
                .map(Token::Char)
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_str() {
        assert_eq!(
            parse_tokens(r#""Hello" "\t\n\r" "World""#),
            vec!["Hello", "\t\n\r", "World"]
                .into_iter()
                .map(|s| Token::Str(s.into()))
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_arithmetic_operators() {
        use super::Token::*;
        assert_eq!(
            parse_tokens("++-*/%,"),
            vec![Plus, Plus, Minus, Star, Slash, Mod, Comma]
        );
        assert_eq!(
            parse_tokens("+ + - * / % ,"),
            vec![Plus, Plus, Minus, Star, Slash, Mod, Comma]
        );
    }

    #[test]
    fn test_ambiguous_operators() {
        use super::Token::*;
        assert_eq!(
            parse_tokens("|| | & && < > >> << >= <="),
            vec![
                LogicalOr,
                BitOr,
                BitAnd,
                LogicalAnd,
                Less,
                Greater,
                ShiftRight,
                ShiftLeft,
                GreaterOrEqual,
                LessOrEqual
            ]
        );
    }

    #[test]
    fn test_directive() {
        use super::Token::*;
        assert_eq!(parse_tokens(".include"), vec![Directive("include")]);
    }
}
