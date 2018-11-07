use combine::error::ParseError;
use combine::stream::Stream;
use combine::{
    attempt, between, chainl1, choice, eof, optional, satisfy_map, sep_by, skip_many1, token,
    Parser,
};

use crate::lexer::Token;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Item {
    Directive(String, Vec<Expr>),
    Instruction(String, Vec<Expr>),
    Label(String),
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    ShiftLeft,
    ShiftRight,
    Equals,
    NotEquals,
    LessThan,
    GreaterThan,
    BitAnd,
    BitXor,
    BitOr,
    LogicalAnd,
    LogicalOr,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum UnaryOperator {
    Minus,
    BitNot,
    LogicalNot,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expr {
    Ident(String),
    String(String),
    Char(u8),
    Int(i64),
    ArgRef(i64),
    Binary(BinaryOperator, Box<Expr>, Box<Expr>),
    Unary(UnaryOperator, Box<Expr>),
    Apply(String, Vec<Expr>),
    Assign(String, Box<Expr>),
    PostInc(String),
}

impl Expr {
    pub fn ident(name: impl Into<String>) -> Self {
        Expr::Ident(name.into())
    }
    pub fn binary(op: BinaryOperator, e1: Expr, e2: Expr) -> Self {
        Expr::Binary(op, Box::new(e1), Box::new(e2))
    }
    pub fn unary(op: UnaryOperator, e: Expr) -> Self {
        Expr::Unary(op, Box::new(e))
    }
    pub fn add(e1: Expr, e2: Expr) -> Self {
        Self::binary(BinaryOperator::Add, e1, e2)
    }
    pub fn subtract(e1: Expr, e2: Expr) -> Self {
        Self::binary(BinaryOperator::Subtract, e1, e2)
    }
    pub fn multiply(e1: Expr, e2: Expr) -> Self {
        Self::binary(BinaryOperator::Multiply, e1, e2)
    }
    pub fn divide(e1: Expr, e2: Expr) -> Self {
        Self::binary(BinaryOperator::Divide, e1, e2)
    }
    pub fn equals(e1: Expr, e2: Expr) -> Self {
        Self::binary(BinaryOperator::Equals, e1, e2)
    }
    pub fn less_than(e1: Expr, e2: Expr) -> Self {
        Self::binary(BinaryOperator::LessThan, e1, e2)
    }
    pub fn greater_than(e1: Expr, e2: Expr) -> Self {
        Self::binary(BinaryOperator::GreaterThan, e1, e2)
    }
    pub fn logical_or(e1: Expr, e2: Expr) -> Self {
        Self::binary(BinaryOperator::LogicalOr, e1, e2)
    }
    pub fn apply(name: impl Into<String>, args: Vec<Expr>) -> Self {
        Expr::Apply(name.into(), args)
    }
    pub fn minus(e: Expr) -> Self {
        Expr::Unary(UnaryOperator::Minus, Box::new(e))
    }
    pub fn post_inc(name: impl Into<String>) -> Self {
        Expr::PostInc(name.into())
    }
    pub fn assign(ident: impl Into<String>, e: Expr) -> Self {
        Expr::Assign(ident.into(), Box::new(e))
    }
}

parser! {
    fn comma_list['a, I]()(I) -> Vec<Expr>
    where [
        I: Stream<Token = Token<'a>>,
        I::Error: ParseError<I::Token, I::Range, I::Position>,
    ]
    {
        sep_by(expr(), token(Token::Comma))
    }
}

struct Line(Option<String>, Option<Item>);

parser! {
    pub fn ident['a, I]()(I) -> String
    where [
        I: Stream<Token = Token<'a>>,
        I::Error: ParseError<I::Token, I::Range, I::Position>,
    ]
    {
        satisfy_map(|t| match t {
            Token::Ident(name) => Some(name.to_string()),
            _ => None,
        })
    }
}

parser! {
    fn line['a, I]()(I) -> Line
    where [
        I: Stream<Token = Token<'a>>,
        I::Error: ParseError<I::Token, I::Range, I::Position>,
    ]
    {
        let directive = satisfy_map(|t| match t {
            Token::Directive(name) => Some(name),
            _ => None,
        }).and(comma_list());
        let instruction = satisfy_map(|t| match t {
            Token::Ident(name) => Some(name),
            _ => None
        }).and(comma_list());
        let label = satisfy_map(|t| match t {
            Token::Ident(name) => Some(name),
            _ => None
        }).skip(token(Token::Colon));
        optional(attempt(label)).and(optional(choice((
            directive.map(|(name, args)| Item::Directive(name.into(), args)),
            instruction.map(|(name, args)| Item::Instruction(name.into(), args)),
        )))).map(|(label, stmt)| Line(label.map(|s| s.into()), stmt))
    }
}

impl Extend<Line> for Vec<Item> {
    fn extend<T>(&mut self, lines: T)
    where
        T: IntoIterator<Item = Line>,
    {
        for line in lines.into_iter() {
            if let Some(label) = line.0 {
                self.push(Item::Label(label));
            }
            if let Some(stmt) = line.1 {
                self.push(stmt);
            }
        }
    }
}

parser! {
    pub fn file['a, I]()(I) -> Vec<Item>
    where [
        I: Stream<Token = Token<'a>>,
        I::Error: ParseError<I::Token, I::Range, I::Position>,
    ]
    {
        sep_by(line(), vspace()).skip(eof())
    }
}

fn vspace<'a, I>() -> impl Parser<I, Output = ()>
where
    I: Stream<Token = Token<'a>>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
{
    skip_many1(token(Token::Eol))
}

// fn factor_<'a, I>() -> impl Parser<Input = I, Output = Expr>
// where
//     I: Stream<Item = Token<'a>>,
//     I::Error: ParseError<I::Item, I::Range, I::Position>,
// {
// }

parser! {
    fn factor['a, I]()(I) -> Expr
    where [
        I: Stream<Token = Token<'a>>,
        I::Error: ParseError<I::Token, I::Range, I::Position>,
    ]
    {
        let arg_ref = || token(Token::At).with(satisfy_map(|t| match t {
            Token::Int(n) => Some(Expr::ArgRef(n)),
            _ => None,
        }));
        let literal = || satisfy_map(|t| match t {
            Token::Int(n) => Some(Expr::Int(n)),
            Token::Str(s) => Some(Expr::String(s)),
            Token::Char(c) => Some(Expr::Char(c)),
            _ => None,
        });
        let ident_or_call = || (ident(),
                                optional(between(token(Token::LParen),
                                                 token(Token::RParen),
                                                 comma_list())))
            .map(|(id, args)| match args {
                Some(args) => Expr::apply(id, args),
                None => Expr::Ident(id),
            });
        let unary_op = choice(((token(Token::Minus)).map(|_| UnaryOperator::Minus),
                               (token(Token::BitNot)).map(|_| UnaryOperator::BitNot)));
        let negated = || unary_op.and(factor()).map(|(op, e)| Expr::unary(op, e));
        choice((
            between(token(Token::LParen), token(Token::RParen), expr()),
            arg_ref(),
            ident_or_call(),
            literal(),
            negated(),
        ))
    }
}

fn binary_op<'a, I>(
    p: impl Parser<I, Output = BinaryOperator>,
) -> impl Parser<I, Output = impl Fn(Expr, Expr) -> Expr>
where
    I: Stream<Token = Token<'a>>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
{
    p.map(|op| move |e1, e2| Expr::binary(op, e1, e2))
}

parser! {
    fn expr['a, I]()(I) -> Expr
    where [
        I: Stream<Token = Token<'a>>,
        I::Error: ParseError<I::Token, I::Range, I::Position>,
    ]
    {
        use self::BinaryOperator::*;

        let term_op = binary_op(choice((token(Token::Star).map(|_| Multiply),
                                        token(Token::Slash).map(|_| Divide),
                                        token(Token::Mod).map(|_| Modulo))));
        let arith_op = binary_op(choice((token(Token::Plus).map(|_| Add),
                                         token(Token::Minus).map(|_| Subtract))));
        let shift_op = binary_op(choice((token(Token::ShiftLeft).map(|_| ShiftLeft),
                                         token(Token::ShiftRight).map(|_| ShiftRight))));
        let cmp_op = binary_op(choice((token(Token::Equals).map(|_| Equals),
                                       token(Token::NotEquals).map(|_| NotEquals),
                                       token(Token::Greater).map(|_| GreaterThan),
                                       token(Token::Less).map(|_| LessThan))));
        let bit_op = binary_op(choice((token(Token::BitAnd).map(|_| BitAnd),
                                       token(Token::BitXor).map(|_| BitXor),
                                       token(Token::BitOr).map(|_| BitOr))));
        let assign_expr = ident().skip(token(Token::Assign))
            .and(expr())
            .map(|(ident, e)| Expr::assign(ident, e));
        let post_inc = ident().skip(token(Token::Plus)).map(Expr::post_inc);
        let term = chainl1(factor(), term_op);
        let arith_expr = chainl1(term, arith_op);
        let shift_expr = chainl1(arith_expr, shift_op);
        let cmp_expr = chainl1(shift_expr, cmp_op);
        let bit_expr = chainl1(cmp_expr, bit_op);
        let logical_and_expr = chainl1(bit_expr, binary_op(token(Token::LogicalAnd).map(|_| LogicalAnd)));
        let logical_or_expr = chainl1(logical_and_expr, binary_op(token(Token::LogicalOr).map(|_| LogicalOr)));
        choice((attempt(assign_expr), attempt(logical_or_expr), post_inc))
    }
}

// parser! {
//     fn expr['a, I]()(I) -> Expr
//     where [
//         I: Stream<Item = Token<'a>>,
//         I::Error: ParseError<I::Item, I::Range, I::Position>,
//     ]
//     {
//         expr_()
//     }
// }

#[cfg(test)]
mod tests {
    use combine::{easy, EasyParser};

    use super::*;
    use crate::lexer;

    #[derive(Debug)]
    enum Error {
        Parse,
        Lex,
    }

    fn lex<'a>(input: &'a str) -> Result<Vec<Token<'a>>, Error> {
        let (tokens, rest): (Vec<Token<'a>>, _) = lexer::tokens()
            .easy_parse(input.as_bytes())
            .map_err(|_| Error::Lex)?;
        assert!(rest.len() == 0);
        Ok(tokens)
    }

    fn parse<'a, 'b, Output, P>(tokens: &'b [Token<'a>], mut p: P) -> Result<Output, Error>
    where
        P: Parser<easy::Stream<&'b [Token<'a>]>, Output = Output>,
        Output: 'a + 'b, //P::Error: ParseError<Token<'a>, &'b [Token<'a>], stream::PointerOffset>,
    {
        let (result, rest) = p.easy_parse(tokens).map_err(|_| Error::Parse)?;
        assert!(rest.len() == 0);
        Ok(result)
    }

    #[test]
    fn test_ident() {
        for input in &["TEST", "TEST123", "TEST_123_bar", "_TEST_123_bar"] {
            assert_eq!(
                parse(&lex(input).expect("lexing failed"), ident()).expect("parsing failed"),
                input.to_string()
            );
        }
    }

    #[test]
    fn test_arg_ref() {
        let tokens = lex("@42").expect("lexing failed");
        assert_eq!(
            parse(&tokens, expr()).expect("parsing failed"),
            Expr::ArgRef(42)
        );
    }

    #[test]
    fn test_logical_and() {
        assert_eq!(
            parse(&lex("a && b").expect("lexing failed"), expr()).expect("parsing failed"),
            Expr::Binary(
                BinaryOperator::LogicalAnd,
                Box::new(Expr::ident("a")),
                Box::new(Expr::ident("b"))
            )
        );
    }

    #[test]
    fn test_logical_or() {
        assert_eq!(
            parse(&lex("a || b").expect("lexing failed"), expr()).expect("parsing failed"),
            Expr::Binary(
                BinaryOperator::LogicalOr,
                Box::new(Expr::ident("a")),
                Box::new(Expr::ident("b"))
            )
        );
    }

    #[test]
    fn test_directive() {
        let tokens = lex(concat!(
            ".device ATmega8\n",
            ".org 0\n",
            ".db 1, 2\n",
            ".db \"Hello\", 32, \"World\"\n"
        ))
        .expect("lexing failed");
        let commands: Vec<_> = parse(&tokens, file()).expect("parsing failed");
        assert_eq!(
            commands,
            vec![
                Item::Directive("device".into(), vec![Expr::ident("ATmega8")]),
                Item::Directive("org".into(), vec![Expr::Int(0)]),
                Item::Directive("db".into(), vec![Expr::Int(1), Expr::Int(2)]),
                Item::Directive(
                    "db".into(),
                    vec![
                        Expr::String("Hello".into()),
                        Expr::Int(32),
                        Expr::String("World".into())
                    ],
                ),
            ]
        );
    }

    #[test]
    fn test_file_eof() {
        let tokens = lex(concat!(".device ATmega8\n", ".db 1, 2")).expect("lexing failed");
        assert_eq!(
            parse(&tokens, file()).expect("parsing failed"),
            vec![
                Item::Directive("device".into(), vec![Expr::ident("ATmega8")]),
                Item::Directive("db".into(), vec![Expr::Int(1), Expr::Int(2)])
            ]
        );
    }

    #[test]
    fn test_directive_if() {
        let tokens = lex(".if (pc>FLASHEND)\n").expect("lexing failed");
        let commands: Vec<_> = parse(&tokens, file()).expect("parsing failed");
        assert_eq!(
            commands,
            vec![Item::Directive(
                "if".into(),
                vec![Expr::greater_than(
                    Expr::ident("pc"),
                    Expr::ident("FLASHEND")
                )]
            ),]
        );
    }

    #[test]
    fn test_directive_if_arg_ref() {
        let tokens = lex(".if (@0-pc > 2040) || (pc-@0>2040)\n").expect("lexing failed");
        assert_eq!(
            parse(&tokens, file()).expect("parsing failed"),
            vec![Item::Directive(
                "if".into(),
                vec![Expr::logical_or(
                    Expr::greater_than(
                        Expr::subtract(Expr::ArgRef(0), Expr::ident("pc")),
                        Expr::Int(2040)
                    ),
                    Expr::greater_than(
                        Expr::subtract(Expr::ident("pc"), Expr::ArgRef(0)),
                        Expr::Int(2040)
                    )
                )]
            )]
        );
    }

    #[test]
    fn test_directive_set() {
        let commands: Vec<_> = parse(
            &lex(".set AMFORTH_NRWW_SIZE=(FLASHEND-AMFORTH_RO_SEG)*2\n").expect("lexing failed"),
            file(),
        )
        .expect("parsing failed");
        assert_eq!(
            commands,
            vec![Item::Directive(
                "set".into(),
                vec![Expr::assign(
                    "AMFORTH_NRWW_SIZE",
                    Expr::multiply(
                        Expr::subtract(Expr::ident("FLASHEND"), Expr::ident("AMFORTH_RO_SEG")),
                        Expr::Int(2)
                    )
                )]
            ),]
        );
    }

    #[test]
    fn test_comment() {
        let tokens = lex(concat!(
            "; A comment at the start\n",
            ".device ATmega8 ; A comment\n",
            ".org 0 ; This is the interrupt vector address\n",
            "; This is a line with just a comment\n"
        ))
        .expect("lexing failed");
        let commands: Vec<_> = parse(&tokens, file()).expect("parsing failed");
        assert_eq!(
            commands,
            vec![
                Item::Directive("device".into(), vec![Expr::ident("ATmega8")]),
                Item::Directive("org".into(), vec![Expr::Int(0)]),
            ]
        );
    }

    #[test]
    fn test_instruction() {
        let tokens = lex(concat!("ldi R16, ';'\n", "ldi R16, 0x3b\n")).expect("lexing failed");
        let commands: Vec<_> = parse(&tokens, file()).expect("parsing failed");
        assert_eq!(
            commands,
            vec![
                Item::Instruction("ldi".into(), vec![Expr::ident("R16"), Expr::Char(b';')]),
                Item::Instruction("ldi".into(), vec![Expr::ident("R16"), Expr::Int(0x3b)]),
            ]
        );
    }

    #[test]
    fn test_label() {
        let tokens = lex(concat!("FOO: .byte 2\n", "BAR: ldi R16, 0\n")).expect("lexing failed");
        let commands: Vec<_> = parse(&tokens, file()).expect("parsing failed");
        assert_eq!(
            commands,
            vec![
                Item::Label("FOO".into()),
                Item::Directive("byte".into(), vec![Expr::Int(2)]),
                Item::Label("BAR".into()),
                Item::Instruction("ldi".into(), vec![Expr::ident("R16"), Expr::Int(0)]),
            ]
        );
    }

    #[test]
    fn test_expr_basic() {
        assert_eq!(
            parse(&lex("TEST + 42 * foo").expect("lexing failed"), expr()).expect("parsing failed"),
            Expr::add(
                Expr::ident("TEST"),
                Expr::multiply(Expr::Int(42), Expr::ident("foo"))
            )
        );
        assert_eq!(
            parse(&lex("TEST / 42 - foo").expect("lexing failed"), expr()).expect("parsing failed"),
            Expr::subtract(
                Expr::divide(Expr::ident("TEST"), Expr::Int(42)),
                Expr::ident("foo")
            )
        );
    }

    #[test]
    fn test_expr_minus() {
        assert_eq!(
            parse(&lex("-42").unwrap(), expr()).expect("parsing failed"),
            Expr::minus(Expr::Int(42))
        );
        assert_eq!(
            parse(&lex("-42 * foo").unwrap(), expr()).expect("parsing failed"),
            Expr::multiply(Expr::minus(Expr::Int(42)), Expr::ident("foo"))
        );
        assert_eq!(
            parse(&lex("TEST + 42 * -foo").unwrap(), expr()).expect("parsing failed"),
            Expr::add(
                Expr::ident("TEST"),
                Expr::multiply(Expr::Int(42), Expr::minus(Expr::ident("foo")))
            )
        );
    }

    #[test]
    fn test_expr_postinc() {
        assert_eq!(
            parse(&lex("Z+").unwrap(), expr()).expect("parsing failed"),
            Expr::post_inc("Z")
        );
    }

    #[test]
    fn test_expr_parens() {
        assert_eq!(
            parse(&lex("(TEST + 42) * foo").unwrap(), expr()).expect("parsing failed"),
            Expr::multiply(
                Expr::add(Expr::ident("TEST"), Expr::Int(42)),
                Expr::ident("foo")
            )
        );
        assert_eq!(
            parse(&lex("TEST / (42 - foo)").unwrap(), expr()).expect("parsing failed"),
            Expr::divide(
                Expr::ident("TEST"),
                Expr::subtract(Expr::Int(42), Expr::ident("foo"))
            )
        );
    }

    #[test]
    fn test_expr_cmp() {
        assert_eq!(
            parse(&lex("(TEST < 42 * foo)").unwrap(), expr()).expect("parsing failed"),
            Expr::less_than(
                Expr::ident("TEST"),
                Expr::multiply(Expr::Int(42), Expr::ident("foo"))
            )
        );
    }

    #[test]
    fn test_expr_equals() {
        assert_eq!(
            parse(&lex("(TEST + 42) == foo").unwrap(), expr()).expect("parsing failed"),
            Expr::equals(
                Expr::add(Expr::ident("TEST"), Expr::Int(42)),
                Expr::ident("foo")
            )
        );
    }

    #[test]
    fn test_expr_apply() {
        assert_eq!(
            parse(&lex("foo (TEST + 42, 23) * bar").unwrap(), expr()).expect("parsing failed"),
            Expr::multiply(
                Expr::apply(
                    "foo",
                    vec![Expr::add(Expr::ident("TEST"), Expr::Int(42)), Expr::Int(23)]
                ),
                Expr::ident("bar")
            )
        );
    }

    #[test]
    fn test_expr_assign() {
        assert_eq!(
            parse(&lex("foo = (TEST + 42) * bar").unwrap(), expr()).expect("parsing failed"),
            Expr::assign(
                "foo",
                Expr::multiply(
                    Expr::add(Expr::ident("TEST"), Expr::Int(42)),
                    Expr::ident("bar")
                )
            )
        );
        assert_eq!(
            parse(
                &lex("AMFORTH_NRWW_SIZE=(FLASHEND-AMFORTH_RO_SEG)*2").unwrap(),
                expr()
            )
            .expect("parsing failed"),
            Expr::assign(
                "AMFORTH_NRWW_SIZE",
                Expr::multiply(
                    Expr::subtract(Expr::ident("FLASHEND"), Expr::ident("AMFORTH_RO_SEG")),
                    Expr::Int(2)
                )
            )
        );
    }
}
