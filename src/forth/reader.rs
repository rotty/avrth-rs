///! A minimalistic parser for a subset of Forth
///
/// This is intended for bootstrapping, allowing to parse the Forth
/// source code that makes up the real implementation of the parser
/// (and the rest of runtime to support it). The avrth
/// implementation's runtime is written in a subset of Forth that can
/// be handled by this "parser". Specifically, the "syntax" of the Forth
/// subset is fixed; there is no way to defined new parsing words from
/// within the parsed code.

pub struct Reader<'a> {
    input: &'a str,
    after_colon: bool,
    pos: usize,
}

impl<'a> Reader<'a> {
    pub fn new(input: &'a str) -> Self {
        Reader {
            input: input,
            after_colon: false,
            pos: 0,
        }
    }

    pub fn tokens<'b>(&'b mut self) -> TokenStream<'a, 'b>
    where
        'a: 'b,
    {
        TokenStream::<'a, 'b> { reader: self }
    }

    pub fn read(&mut self) -> Option<Token<'a>> {
        let bracketed = |reader: &mut Self, c, variant: fn(&'a str) -> Token<'a>| {
            if reader.pos == reader.input.len() {
                unimplemented!(); // TODO: unterminated string literal/comment
            }
            if let Some(end) = reader.input[reader.pos + 1..].find(c) {
                let end_pos = reader.pos + 1 + end;
                let content = &reader.input[reader.pos + 1..end_pos];
                reader.pos = end_pos + 1;
                Some(variant(content))
            } else {
                unimplemented!(); // TODO: unterminated string literal/comment
            }
        };
        if let Some(offset) = self.input[self.pos..].find(|c: char| !c.is_whitespace()) {
            let start = self.pos + offset;
            let len = self.input[start..]
                .find(char::is_whitespace)
                .unwrap_or(self.input.len() - start);
            let token = &self.input[start..start + len];
            self.pos = start + len;
            match (token, self.after_colon) {
                (":", false) => {
                    self.after_colon = true;
                    Some(Token::Ident(":"))
                }
                ("\\", false) => {
                    if self.pos == self.input.len() {
                        Some(Token::Comment("")) // TODO: correct?
                    } else if let Some(eol) = self.input[self.pos..].find('\n') {
                        let end_pos = self.pos + eol;
                        let start_pos = if eol == 0 { self.pos } else { self.pos + 1 };
                        let comment = &self.input[start_pos..end_pos];
                        self.pos = end_pos + 1;
                        Some(Token::Comment(comment))
                    } else {
                        let comment = &self.input[self.pos + 1..self.input.len()];
                        self.pos = self.input.len();
                        Some(Token::Comment(comment))
                    }
                }
                ("(", false) => bracketed(self, ')', Token::Comment),
                ("s\"", false) => bracketed(self, '"', Token::String),
                _ => {
                    self.after_colon = false;
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

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Token<'a> {
    Ident(&'a str),
    Int(isize),
    String(&'a str),
    Comment(&'a str),
}

pub struct TokenStream<'a, 'b>
where
    'a: 'b,
{
    reader: &'b mut Reader<'a>,
}

impl<'a, 'b> Iterator for TokenStream<'a, 'b> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.reader.read()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn read_vec(input: &str) -> Vec<Token> {
        Reader::new(input).tokens().collect()
    }

    #[test]
    fn empty_input() {
        assert_eq!(read_vec(""), vec![]);
    }

    #[test]
    fn single_int() {
        assert_eq!(read_vec("-42"), vec![Token::Int(-42)]);
    }

    #[test]
    fn simple_multi() {
        assert_eq!(
            read_vec("hello world -42 @"),
            vec![
                Token::Ident("hello"),
                Token::Ident("world"),
                Token::Int(-42),
                Token::Ident("@"),
            ]
        );
    }

    #[test]
    fn string_literal() {
        assert_eq!(
            read_vec(r#"hello s" this is a string" -42"#),
            vec![
                Token::Ident("hello"),
                Token::String("this is a string"),
                Token::Int(-42),
            ]
        );
    }

    #[test]
    fn line_comment() {
        assert_eq!(
            read_vec(
                r#"
42 \ The answer to life, the universe and everything
23 \ A prime number"#
            ),
            vec![
                Token::Int(42),
                Token::Comment("The answer to life, the universe and everything"),
                Token::Int(23),
                Token::Comment("A prime number"),
            ]
        );
    }

    #[test]
    fn line_comment_empty() {
        assert_eq!(
            read_vec(
                r#"\
42"#
            ),
            vec![Token::Comment(""), Token::Int(42)]
        );
    }

    #[test]
    fn paren_comment() {
        assert_eq!(
            read_vec(r#": foo ( n -- n) 42 + ;"#),
            vec![
                Token::Ident(":"),
                Token::Ident("foo"),
                Token::Comment("n -- n"),
                Token::Int(42),
                Token::Ident("+"),
                Token::Ident(";"),
            ]
        );
    }

    #[test]
    fn define_backslash() {
        assert_eq!(
            read_vec(r#": \ 42 ;"#),
            vec![
                Token::Ident(":"),
                Token::Ident("\\"),
                Token::Int(42),
                Token::Ident(";"),
            ]
        );
    }

    #[test]
    fn define_bracket_open() {
        assert_eq!(
            read_vec(r#": ( 42 ;"#),
            vec![
                Token::Ident(":"),
                Token::Ident("("),
                Token::Int(42),
                Token::Ident(";"),
            ]
        );
    }

    #[test]
    fn define_s_quote() {
        assert_eq!(
            read_vec(r#": s" 42 ;"#),
            vec![
                Token::Ident(":"),
                Token::Ident("s\""),
                Token::Int(42),
                Token::Ident(";"),
            ]
        );
    }
}
