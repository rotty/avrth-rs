use combine::char::{alpha_num, char, crlf, digit, hex_digit, letter, newline, string};
use combine::error::{ParseError, StreamError};
use combine::stream::{Stream, StreamOnce};
use combine::parser::repeat::skip_until;
use combine::{any, between, chainl1, choice, eof, many, many1, none_of, optional, satisfy, sep_by, skip_many, skip_many1, r#try, Parser};

#[derive(Debug, Eq, PartialEq)]
pub enum Command {
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
    Equals,
    NotEquals,
    LessThan,
    GreaterThan,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum UnaryOperator {
    Negate,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Expr {
    Ident(String),
    String(String),
    Char(char),
    Int(i64),
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
    pub fn apply(name: impl Into<String>, args: Vec<Expr>) -> Self {
        Expr::Apply(name.into(), args)
    }
    pub fn negate(e: Expr) -> Self {
        Expr::Unary(UnaryOperator::Negate, Box::new(e))
    }
    pub fn post_inc(name: impl Into<String>) -> Self {
        Expr::PostInc(name.into())
    }
    pub fn assign(ident: impl Into<String>, e: Expr) -> Self {
        Expr::Assign(ident.into(), Box::new(e))
    }
}

fn lex_char<I>(c: char) -> impl Parser<Input = I, Output = char>
where
    I: Stream<Item = char>,
    I::Error: ParseError<char, I::Range, I::Position>,
{
    char(c).skip(hspace())
}

parser! {
    fn comma_list[I]()(I) -> Vec<Expr>
    where [
        I: Stream<Item = char>,
        I::Error: ParseError<char, I::Range, I::Position>,
    ]
    {
        sep_by(expr(), lex_char(','))
    }
}

struct Line(Option<String>, Option<Command>);

pub fn line<I>() -> impl Parser<Input = I, Output = Line>
where
    I: Stream<Item = char>,
    I::Error: ParseError<char, I::Range, I::Position>,
{
    let args = || hspace().with(comma_list());
    let directive = char('.').with(ident()).and(args());
    let instruction = ident().and(args());
    let label = ident().skip(char(':'));
    optional(r#try(label)).skip(hspace()).and(optional(choice((
        directive.map(|(name, args)| Command::Directive(name, args)),
        instruction.map(|(name, args)| Command::Instruction(name, args)),
    )))).skip(hspace()).map(|(label, stmt)| Line(label, stmt))
}

impl Extend<Line> for Vec<Command> {
    fn extend<T>(&mut self, lines: T)
    where
        T: IntoIterator<Item = Line>
    {
        for line in lines.into_iter() {
            if let Some(label) = line.0 {
                self.push(Command::Label(label));
            }
            if let Some(stmt) = line.1 {
                self.push(stmt);
            }
        }
    }
}

pub fn file<I>() -> impl Parser<Input = I, Output = Vec<Command>>
where
    I: Stream<Item = char>,
    I::Error: ParseError<char, I::Range, I::Position>,
{
    sep_by(line(), vspace()).skip(eof())
}

fn ident<I>() -> impl Parser<Input = I, Output = String>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    many1(letter().or(char('_'))).and(many(alpha_num().or(char('_')))).map(|(mut prefix, suffix): (String, String)| {
        prefix.extend(suffix.chars());
        prefix
    })
}

fn int_literal<I>() -> impl Parser<Input = I, Output = i64>
where
    I: Stream<Item = char>,
    I::Error: ParseError<char, I::Range, I::Position>,
{
    let decimal_digits = || many1(digit()).map(|digits| (10, digits));
    let hex_digits = || choice((char('$').map(|_| ()),
                                r#try(string("0x")).map(|_| ())))
        .with(many1(hex_digit())).map(|digits| (16, digits));
    choice((hex_digits(), decimal_digits()))
    // This monstrous type is there to guide the type checker
    // is required to avoid imposing a From<ParseIntError>
    // bound on all parsers that call `int_literal`.
        .and_then(|(base, digits): ((u32, String))| -> Result<i64, <<I as StreamOnce>::Error as ParseError<I::Item, I::Range, I::Position>>::StreamError> {
            match i64::from_str_radix(&digits, base) {
                Ok(value) => Ok(value),
                Err(_) => Err(StreamError::message_message("integer out of bounds")),
            }
        })
}

fn hspace<I>() -> impl Parser<Input = I, Output = ()>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let comment = char(';').with(skip_until(satisfy(|c| c == '\n' || c == '\r')));
    skip_many(satisfy(|c: char| c.is_whitespace() && c != '\n' && c != '\r')).map(|_| ())
        .skip(optional(comment)).silent()
}

fn vspace<I>() -> impl Parser<Input = I, Output = ()>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let eol = || crlf().or(newline()).map(|_| ());
    skip_many1(eol().skip(hspace()))
}

fn escaped_char<I>(terminators: &'static [char]) -> impl Parser<Input = I, Output = char>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    choice((
        char('\\').with(any().map(|escaped| match escaped {
            'n' => '\n',
            't' => '\t',
            _ => escaped,
        })),
        none_of(terminators.iter().cloned())
    ))
}

fn factor_<I>() -> impl Parser<Input = I, Output = Expr>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let string_literal = || between(char('"'), char('"'), many(escaped_char(&['"'])));
    let char_literal = || between(char('\''), char('\''), escaped_char(&['\'']));
    let literal = || choice((
        int_literal().map(Expr::Int),
        string_literal().map(Expr::String),
        char_literal().map(Expr::Char),
    ));
    let ident_or_call = || (ident().skip(hspace()), optional(between(char('('), char(')'), comma_list())))
        .map(|(id, args)| match args {
            Some(args) => Expr::apply(id, args),
            None => Expr::Ident(id),
        });
    let negated = || char('-').with(factor()).map(Expr::negate);
    hspace().with(choice((
        between(char('('), char(')'), expr()),
        ident_or_call(),
        literal(),
        negated(),
    ))).skip(hspace())
}

parser!{
    fn factor[I]()(I) -> Expr
    where [
        I: Stream<Item = char>,
        I::Error: ParseError<char, I::Range, I::Position>,
    ]
    {
        factor_()
    }
}

fn binary_op<I>(p: impl Parser<Input = I, Output = BinaryOperator>) -> impl Parser<Input = I, Output = impl Fn(Expr, Expr) -> Expr>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    p.map(|op| move |e1, e2| Expr::binary(op, e1, e2))
}

fn expr_<I>() -> impl Parser<Input = I, Output = Expr>
where
    I: Stream<Item = char>,
    I::Error: ParseError<char, I::Range, I::Position>,
{
    use self::BinaryOperator::*;

    let term_op = binary_op(choice((char('*').map(|_| Multiply),
                                    char('/').map(|_| Divide))));
    let arith_op = binary_op(choice((char('+').map(|_| Add),
                                     char('-').map(|_| Subtract))));
    let cmp_op = binary_op(choice((string("==").map(|_| Equals),
                                   string("!=").map(|_| NotEquals),
                                   char('>').map(|_| GreaterThan),
                                   char('<').map(|_| LessThan))));
    let assign_expr = ident().skip(hspace())
        .skip(lex_char('='))
        .and(expr())
        .map(|(ident, e)| Expr::assign(ident, e));
    let post_inc = ident().skip(lex_char('+')).map(Expr::post_inc);
    let term = chainl1(factor(), term_op);
    let arith_expr = chainl1(term, arith_op);
    let cmp_expr = chainl1(arith_expr, cmp_op);
    choice((r#try(assign_expr), r#try(cmp_expr), post_inc))
}

parser!{
    fn expr[I]()(I) -> Expr
    where [
        I: Stream<Item = char>,
        I::Error: ParseError<char, I::Range, I::Position>,
    ]
    {
        expr_()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hspace() {
        assert_eq!(hspace().easy_parse("").expect("parsing failed"),
                   ((), ""));
        assert_eq!(hspace().easy_parse("\n").expect("parsing failed"),
                   ((), "\n"));
        assert_eq!(hspace().easy_parse("\t  \n").expect("parsing failed"),
                   ((), "\n"));
    }

    #[test]
    fn test_vspace() {
        assert_eq!(vspace().easy_parse("\n").expect("parsing failed"),
                   ((), ""));
        assert_eq!(vspace().easy_parse("\n; A comment with EOL\n").expect("parsing failed"),
                   ((), ""));
        assert_eq!(vspace().easy_parse("\n; A comment with EOL\n  ").expect("parsing failed"),
                   ((), ""));
    }

    #[test]
    fn test_ident() {
        assert_eq!(ident().easy_parse("TEST").expect("parsing failed"),
                   ("TEST".into(), ""));
        assert_eq!(ident().easy_parse("TEST123").expect("parsing failed"),
                   ("TEST123".into(), ""));
        assert_eq!(ident().easy_parse("TEST_123_bar").expect("parsing failed"),
                   ("TEST_123_bar".into(), ""));
        assert_eq!(ident().easy_parse("_TEST_123_bar").expect("parsing failed"),
                   ("_TEST_123_bar".into(), ""));
    }

    #[test]
    fn test_directive() {
        let (commands, rest): (Vec<_>, _) = file().easy_parse(
            concat!(".device ATmega8\n",
                    ".org 0\n",
                    ".db 1, 2\n",
                    ".db \"Hello\", 32, \"World\"\n")).expect("parsing failed");
        assert_eq!(commands, vec![
            Command::Directive("device".into(), vec![Expr::ident("ATmega8")]),
            Command::Directive("org".into(), vec![Expr::Int(0)]),
            Command::Directive("db".into(), vec![Expr::Int(1), Expr::Int(2)]),
            Command::Directive("db".into(), vec![
                Expr::String("Hello".into()),
                Expr::Int(32),
                Expr::String("World".into())],
            ),
        ]);
        assert_eq!(rest, "");
    }

    #[test]
    fn test_file_eof() {
        assert_eq!(file().easy_parse(concat!(".device ATmega8\n",
                                             ".db 1, 2")).expect("parsing failed"),
                   (vec![
                       Command::Directive("device".into(), vec![Expr::ident("ATmega8")]),
                       Command::Directive("db".into(), vec![Expr::Int(1), Expr::Int(2)])
                   ],
                    ""));
    }

    #[test]
    fn test_directive_if() {
        let (commands, rest): (Vec<_>, _) = file().easy_parse(
            ".if (pc>FLASHEND)\n").expect("parsing failed");
        assert_eq!(commands, vec![
            Command::Directive("if".into(),
                               vec![Expr::greater_than(Expr::ident("pc"),
                                                       Expr::ident("FLASHEND"))]),
        ]);
        assert_eq!(rest, "");
    }

    #[test]
    fn test_directive_set() {
        let (commands, rest): (Vec<_>, _) = file().easy_parse(
            ".set AMFORTH_NRWW_SIZE=(FLASHEND-AMFORTH_RO_SEG)*2\n").expect("parsing failed");
        assert_eq!(commands, vec![
            Command::Directive("set".into(),
                               vec![Expr::assign("AMFORTH_NRWW_SIZE",
                                                 Expr::multiply(Expr::subtract(Expr::ident("FLASHEND"),
                                                                               Expr::ident("AMFORTH_RO_SEG")),
                                                                Expr::Int(2)))]),
        ]);
        assert_eq!(rest, "");
    }

    #[test]
    fn test_comment() {
        let (commands, rest): (Vec<_>, _) = file().easy_parse(
            concat!("; A comment at the start\n",
                    ".device ATmega8 ; A comment\n",
                    ".org 0 ; This is the interrupt vector address\n",
                    "; This is a line with just a comment\n")).expect("parsing failed");
        assert_eq!(commands, vec![
            Command::Directive("device".into(), vec![Expr::ident("ATmega8")]),
            Command::Directive("org".into(), vec![Expr::Int(0)]),
        ]);
        assert_eq!(rest, "");
    }

    #[test]
    fn test_instruction() {
        let (commands, rest): (Vec<_>, _) = file().easy_parse(
            concat!("ldi R16, ';'\n",
                    "ldi R16, 0x3b\n")).expect("parsing failed");
        assert_eq!(commands, vec![
            Command::Instruction("ldi".into(),
                                   vec![Expr::ident("R16"), Expr::Char(';')]),
            Command::Instruction("ldi".into(),
                                   vec![Expr::ident("R16"), Expr::Int(0x3b)]),
        ]);
        assert_eq!(rest, "");
    }

    #[test]
    fn test_label() {
        let (commands, rest): (Vec<_>, _) = file().easy_parse(
            concat!("FOO: .byte 2\n",
                    "BAR: ldi R16, 0\n")).expect("parsing failed");
        assert_eq!(commands, vec![
            Command::Label("FOO".into()),
            Command::Directive("byte".into(), vec![Expr::Int(2)]),
            Command::Label("BAR".into()),
            Command::Instruction("ldi".into(), vec![Expr::ident("R16"), Expr::Int(0)]),
        ]);
        assert_eq!(rest, "");
    }

    #[test]
    fn test_expr_basic() {
        assert_eq!(expr().easy_parse("TEST + 42 * foo").expect("parsing failed"),
                   (Expr::add(Expr::ident("TEST"),
                              Expr::multiply(Expr::Int(42), Expr::ident("foo"))),
                    ""));
        assert_eq!(expr().easy_parse("TEST / 42 - foo").expect("parsing failed"),
                   (Expr::subtract(Expr::divide(Expr::ident("TEST"), Expr::Int(42)),
                                   Expr::ident("foo")),
                    ""));
    }

    #[test]
    fn test_expr_negate() {
        assert_eq!(expr().easy_parse("-42").expect("parsing failed"),
                   (Expr::negate(Expr::Int(42)),
                    ""));
        assert_eq!(expr().easy_parse("-42 * foo").expect("parsing failed"),
                   (Expr::multiply(Expr::negate(Expr::Int(42)), Expr::ident("foo")),
                    ""));
        assert_eq!(expr().easy_parse("TEST + 42 * -foo").expect("parsing failed"),
                   (Expr::add(Expr::ident("TEST"),
                              Expr::multiply(Expr::Int(42), Expr::negate(Expr::ident("foo")))),
                    ""));
    }

    #[test]
    fn test_expr_postinc() {
        assert_eq!(expr().easy_parse("Z+").expect("parsing failed"),
                   (Expr::post_inc("Z"),
                    ""));
    }

    #[test]
    fn test_expr_parens() {
        assert_eq!(expr().easy_parse("(TEST + 42) * foo").expect("parsing failed"),
                   (Expr::multiply(Expr::add(Expr::ident("TEST"), Expr::Int(42)),
                                   Expr::ident("foo")),
                    ""));
        assert_eq!(expr().easy_parse("TEST / (42 - foo)").expect("parsing failed"),
                   (Expr::divide(Expr::ident("TEST"),
                                 Expr::subtract(Expr::Int(42), Expr::ident("foo"))),
                    ""));
    }

    #[test]
    fn test_expr_cmp() {
        assert_eq!(expr().easy_parse("(TEST < 42 * foo)").expect("parsing failed"),
                   (Expr::less_than(Expr::ident("TEST"),
                                    Expr::multiply(Expr::Int(42), Expr::ident("foo"))),
                    ""));
    }

    #[test]
    fn test_expr_equals() {
        assert_eq!(expr().easy_parse("(TEST + 42) == foo").expect("parsing failed"),
                   (Expr::equals(Expr::add(Expr::ident("TEST"), Expr::Int(42)),
                                 Expr::ident("foo")),
                    ""));
    }

    #[test]
    fn test_expr_apply() {
        assert_eq!(expr().easy_parse("foo (TEST + 42, 23) * bar").expect("parsing failed"),
                   (Expr::multiply(
                       Expr::apply("foo",
                                   vec![Expr::add(Expr::ident("TEST"), Expr::Int(42)),
                                        Expr::Int(23)]),
                       Expr::ident("bar")),
                    ""));
    }

    #[test]
    fn test_expr_assign() {
        assert_eq!(expr().easy_parse("foo = (TEST + 42) * bar").expect("parsing failed"),
                   (Expr::assign("foo",
                                 Expr::multiply(
                                     Expr::add(Expr::ident("TEST"), Expr::Int(42)),
                                     Expr::ident("bar"))),
                    ""));

        assert_eq!(expr().easy_parse("AMFORTH_NRWW_SIZE=(FLASHEND-AMFORTH_RO_SEG)*2").expect("parsing failed"),
                   (Expr::assign("AMFORTH_NRWW_SIZE",
                                 Expr::multiply(
                                     Expr::subtract(Expr::ident("FLASHEND"),
                                                    Expr::ident("AMFORTH_RO_SEG")),
                                     Expr::Int(2))),
                    ""));
    }
}
