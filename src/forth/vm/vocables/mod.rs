use std::collections::HashMap;
use std::fmt;
use std::fs;
use std::path::PathBuf;

use byteorder::ByteOrder;
use failure::{Error, ResultExt};

use forth::reader::{Reader, Token};
use forth::vm::{Cell, Interpreter};

#[macro_use]
mod macros;
pub mod compiler;
pub mod compiler_high;
pub mod derived;
pub mod prim;
pub mod repl;
pub mod store;

pub struct SourceArena {
    table: HashMap<Vec<String>, String>,
}

impl SourceArena {
    pub fn new() -> SourceArena {
        SourceArena {
            table: HashMap::new(),
        }
    }
    pub fn load<'a>(&'a mut self, path: &[&str]) -> Result<&'a str, Error> {
        use std::collections::hash_map::Entry;
        // TODO: better way of finding the root directory instead of
        // relying on working directory.
        let mut buf = PathBuf::new();
        buf.push("src");
        buf.extend(path);
        let full_path = buf.as_path();
        let key = path.iter().map(|s| s.to_string()).collect();
        let entry: Entry<'a, Vec<String>, _> = self.table.entry(key);
        match entry {
            Entry::Occupied(entry) => Ok(entry.into_mut()),
            Entry::Vacant(entry) => {
                let content = fs::read_to_string(full_path).with_context(|_| {
                    format!("while reading {}", full_path.display())
                })?;
                Ok(entry.insert(content))
            }
        }
    }
}

pub enum Vocable<'a, C: Cell, B: ByteOrder> {
    Primitive {
        // TODO: assembly
        run: Interpreter<C, B>,
    },
    Forth {
        immediate: bool,
        code: Vec<Token<'a>>,
    },
}

impl<'a, C: Cell, B: ByteOrder> fmt::Debug for Vocable<'a, C, B> {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            Vocable::Primitive { run: _ } => {
                formatter.write_str("Vocable::Primitive")?;
            }
            Vocable::Forth {
                immediate,
                ref code,
            } => {
                formatter.write_str("Vocable::Forth { ")?;
                formatter.write_fmt(format_args!("immediate={}, code={:?}", immediate, code))?;
                formatter.write_str("}")?;
            }
        }
        Ok(())
    }
}

/// An ordered collection of vocables
pub struct Vocabulary<'a, C: Cell, B: ByteOrder> {
    vocables: HashMap<String, Vocable<'a, C, B>>,
    names: Vec<String>,
}

impl<'a, C: Cell, B: ByteOrder> Vocabulary<'a, C, B> {
    pub fn new() -> Self {
        Vocabulary {
            vocables: HashMap::new(),
            names: Vec::new(),
        }
    }

    pub fn define_primitive(&mut self, name: &str, run: Interpreter<C, B>) {
        self.vocables
            .insert(name.into(), Vocable::Primitive { run: run });
        self.names.push(name.into());
    }

    pub fn load_forth_words(&mut self, arena: &'a mut SourceArena, path: &[&str])
                            -> Result<(), Error> {
        let mut reader = Reader::new(arena.load(path)?);
        self.define_forth_words(&mut reader)?;
        Ok(())
    }

    pub fn define_forth_words(&mut self, reader: &mut Reader<'a>) -> Result<(), Error> {
        #[derive(Copy, Clone, Debug)]
        enum State<'a> {
            TopLevel,
            WordName,
            InWord(&'a str),
            AfterWord(&'a str),
        }

        let mut state = State::TopLevel;
        let mut code = vec![];
        let mut flush = |name: &str, code: Vec<_>| {
            self.vocables.insert(
                name.into(),
                Vocable::Forth {
                    immediate: false,
                    code: code,
                },
            );
            self.names.push(name.into());
        };
        for token in reader.tokens() {
            match (token, state) {
                (Token::Comment(_), _) => {}
                (Token::Ident(":"), State::TopLevel) => {
                    state = State::WordName;
                }
                (Token::Ident(":"), State::AfterWord(name)) => {
                    flush(name, code);
                    code = vec![];
                    state = State::WordName;
                }
                (Token::Ident(";"), State::InWord(name)) => {
                    state = State::AfterWord(name);
                }
                (Token::Ident("immediate"), State::AfterWord(name)) => {
                    flush(name, code);
                    code = vec![];
                    state = State::TopLevel;
                }
                (Token::Ident(name), State::WordName) => {
                    state = State::InWord(name);
                }
                (token, State::InWord(_)) => {
                    code.push(token);
                }
                (token, _) => {
                    return Err(format_err!(
                        "unexpected token {:?} in state {:?}",
                        token,
                        state
                    ));
                }
            }
        }
        match state {
            State::TopLevel => Ok(()),
            State::AfterWord(name) => {
                flush(name, code);
                Ok(())
            }
            _ => Err(format_err!("unterminated word definition"))
        }
    }

    pub fn iter(&self) -> VocabularyIterator<C, B> {
        VocabularyIterator {
            vocabulary: self,
            index: 0,
        }
    }
}

pub struct VocabularyIterator<'a, 'b, C: Cell, B: ByteOrder + 'a>
where
    'a: 'b,
{
    vocabulary: &'b Vocabulary<'a, C, B>,
    index: usize,
}

impl<'a, 'b, C: Cell, B: ByteOrder> Iterator for VocabularyIterator<'a, 'b, C, B> {
    type Item = (&'b str, &'b Vocable<'a, C, B>);

    fn next(&mut self) -> Option<Self::Item> {
        let index = self.index;
        if index == self.vocabulary.names.len() {
            None
        } else {
            let name = &self.vocabulary.names[index];
            self.index = index + 1;
            Some((name, &self.vocabulary.vocables[name]))
        }
    }
}
