use std::collections::HashMap;

use byteorder::ByteOrder;

use forth::parser;
use forth::vm::{Cell, Interpreter};

pub struct SourceArena {}

impl SourceArena {
    pub fn new() -> SourceArena {
        SourceArena {}
    }
}

pub enum Vocable<'a, C: Cell, B: ByteOrder> {
    Primitive {
        run: Interpreter<C, B>,
    }, // TODO: assembly
    Forth {
        immediate: bool,
        code: Vec<parser::Token<'a>>,
    },
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
