use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::Cell;

struct Word<C> {
    name: String,
    immediate: bool,
    hidden: bool,
    xt: C,
}

pub struct WordList<C>(HashMap<String, Word<C>>);

impl<C> WordList<C> {
    pub fn new() -> Self {
        WordList(HashMap::new())
    }
}

pub struct Dict<C: Cell> {
    start: C,
    here_start: C,
    word_lists: Vec<Rc<RefCell<WordList<C>>>>,
    immediate_word_lists: Vec<Rc<RefCell<WordList<C>>>>,
}

impl<C: Cell> Dict<C> {
    pub fn new<'a, W, I>(start: C, here_start: C, word_lists: W, immediate_word_lists: I) -> Self
    where
        W: IntoIterator<Item = &'a Rc<RefCell<WordList<C>>>>,
        I: IntoIterator<Item = &'a Rc<RefCell<WordList<C>>>>,
        C: 'a,
    {
        Dict {
            start: start,
            here_start: here_start,
            word_lists: word_lists.into_iter().map(Rc::clone).collect(),
            immediate_word_lists: immediate_word_lists.into_iter().map(Rc::clone).collect(),
        }
    }
}
