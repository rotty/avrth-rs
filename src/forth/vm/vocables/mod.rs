use std::collections::HashMap;
use std::fmt;

use anyhow::anyhow;
use byteorder::ByteOrder;

use crate::forth::reader::{Reader, Token};
use crate::forth::vm::{Cell, Primitive};

#[macro_use]
mod macros;
pub mod compiler;
pub mod compiler_high;
pub mod control;
pub mod derived;
pub mod file;
pub mod io;
pub mod prim;
pub mod repl;
pub mod store;

pub enum Vocable<'a, C: Cell, B: ByteOrder> {
    Primitive {
        // TODO: assembly
        run: Primitive<C, B>,
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
#[derive(Debug)]
pub struct Vocabulary<'a, C: Cell, B: ByteOrder> {
    vocables: HashMap<String, Vocable<'a, C, B>>,
    names: Vec<String>,
}

impl<'a, C: Cell, B: ByteOrder> Default for Vocabulary<'a, C, B> {
    fn default() -> Self {
        Vocabulary {
            vocables: HashMap::new(),
            names: Vec::new(),
        }
    }
}

impl<'a, C: Cell, B: ByteOrder> Vocabulary<'a, C, B> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn define_primitive(&mut self, name: &str, run: Primitive<C, B>) {
        self.vocables
            .insert(name.into(), Vocable::Primitive { run });
        self.names.push(name.into());
    }

    pub fn define_forth_word(&mut self, name: &str, immediate: bool, code: Vec<Token<'a>>) {
        self.vocables
            .insert(name.into(), Vocable::Forth { immediate, code });
        self.names.push(name.into());
    }

    pub fn define_forth_words(&mut self, code: &'a str) -> anyhow::Result<()> {
        #[derive(Copy, Clone, Debug)]
        enum State<'a> {
            TopLevel,
            WordName,
            InWord(&'a str),
            AfterWord(&'a str),
        }
        let mut reader = Reader::new(code);
        let mut state = State::TopLevel;
        let mut code = vec![];
        for token in reader.tokens() {
            match (token, state) {
                (Token::Comment(_), _) => {}
                (Token::Ident(":"), State::TopLevel) => {
                    state = State::WordName;
                }
                (Token::Ident(":"), State::AfterWord(name)) => {
                    self.define_forth_word(name, false, code);
                    code = vec![];
                    state = State::WordName;
                }
                (Token::Ident(";"), State::InWord(name)) => {
                    state = State::AfterWord(name);
                }
                (Token::Ident("immediate"), State::AfterWord(name)) => {
                    self.define_forth_word(name, true, code);
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
                    return Err(anyhow!("unexpected token {:?} in state {:?}", token, state));
                }
            }
        }
        match state {
            State::TopLevel => Ok(()),
            State::AfterWord(name) => {
                self.define_forth_word(name, false, code);
                Ok(())
            }
            _ => Err(anyhow!("unterminated word definition")),
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
