///! A minimalistic parser for a subset of Forth
///
/// This is intended for bootstrapping, allowing to parse the Forth
/// source code that makes up the real implementation of the parser
/// (and the rest of runtime to support it). The avrth
/// implementation's runtime is written in a subset of Forth that can
/// be handled by this parser. Specifically, the "syntax" of the Forth
/// subset is fixed; there is no way to defined new parsing words from
/// within the parsed code.

pub struct Parser<'a> {
    input: &'a str,
    pos: usize,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Parser {
            input: input,
            pos: 0,
        }
    }

    pub fn token_iter<'b>(&'b mut self) -> TokenStream<'a, 'b>
    where
        'a: 'b,
    {
        TokenStream::<'a, 'b> { parser: self }
    }

    pub fn read(&mut self) -> Option<Token<'a>> {
        if let Some(offset) = self.input[self.pos..].find(|c: char| !c.is_whitespace()) {
            let start = self.pos + offset;
            let len = self.input[start..]
                .find(char::is_whitespace)
                .unwrap_or(self.input.len() - start);
            let token = &self.input[start..start + len];
            self.pos = start + len;
            match token {
                "s\"" => {
                    if self.pos == self.input.len() {
                        unimplemented!(); // TODO: unterminated string literal
                    }
                    if let Some(end) = self.input[self.pos + 1..].find('"') {
                        let end_pos = self.pos + 1 + end;
                        let literal = &self.input[self.pos + 1..end_pos];
                        self.pos = end_pos + 1;
                        Some(Token::StringLiteral(literal))
                    } else {
                        unimplemented!(); // TODO: unterminated string literal
                    }
                }
                _ => {
                    if let Some(n) = token.parse().ok() {
                        Some(Token::Int(n))
                    } else {
                        Some(Token::Ident(token))
                    }
                }
            }
        } else {
            None
        }
    }
}

#[derive(Eq, PartialEq, Debug)]
pub enum Token<'a> {
    Ident(&'a str),
    Int(isize),
    StringLiteral(&'a str),
}

pub struct TokenStream<'a, 'b>
where
    'a: 'b,
{
    parser: &'b mut Parser<'a>,
}

impl<'a, 'b> Iterator for TokenStream<'a, 'b> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.parser.read()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_vec(input: &str) -> Vec<Token> {
        Parser::new(input).token_iter().collect()
    }

    #[test]
    fn empty_input() {
        assert_eq!(parse_vec(""), vec![]);
    }

    #[test]
    fn single_int() {
        assert_eq!(parse_vec("-42"), vec![Token::Int(-42)]);
    }

    #[test]
    fn simple_multi() {
        assert_eq!(
            parse_vec("hello world -42 @"),
            vec![
                Token::Ident("hello"),
                Token::Ident("world"),
                Token::Int(-42),
                Token::Ident("@"),
            ]
        );
    }

    #[test]
    fn test_string_literal() {
        assert_eq!(
            parse_vec(r#"hello s" this is a string" -42"#),
            vec![
                Token::Ident("hello"),
                Token::StringLiteral("this is a string"),
                Token::Int(-42),
            ]
        );
    }
}
