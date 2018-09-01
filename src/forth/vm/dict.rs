use std::cell::{RefCell, RefMut};
use std::collections::HashMap;
use std::rc::Rc;

use super::Cell;

#[derive(Copy, Clone, Debug)]
pub struct Word<C> {
    pub immediate: bool,
    pub hidden: bool,
    pub xt: C,
}

pub struct WordList<C>(HashMap<String, Word<C>>);

impl<C> WordList<C> {
    pub fn new() -> Self {
        WordList(HashMap::new())
    }

    pub fn define(&mut self, name: &str, xt: C) {
        self.0.insert(
            name.into(),
            Word {
                immediate: false,
                hidden: false,
                xt: xt,
            },
        );
    }
}

pub struct Dict<C: Cell> {
    start: C,
    here_start: C,
    word_lists: Vec<Rc<RefCell<WordList<C>>>>,
    immediate_word_lists: Vec<Rc<RefCell<WordList<C>>>>,
    dp: C,
    here: C,
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
            dp: start,
            here: here_start,
        }
    }

    pub fn dp(&self) -> C {
        self.dp
    }

    pub fn set_dp(&mut self, address: C) {
        self.dp = address
    }

    fn find_word<P>(
        predicate: P,
        word_lists: &[Rc<RefCell<WordList<C>>>],
        name: &str,
    ) -> Option<Word<C>>
    where
        P: Fn(&Word<C>) -> bool,
    {
        for word_list in word_lists.iter() {
            let words = word_list.borrow();
            if let Some(word) = words.0.get(name) {
                if !word.hidden && predicate(word) {
                    return Some(*word);
                }
            }
        }
        None
    }

    pub fn get(&self, name: &str, immediate_mode: bool) -> Option<Word<C>> {
        Self::find_word(|_| true, &self.word_lists, name).or_else(|| {
            if immediate_mode {
                Self::find_word(|_| true, &self.immediate_word_lists, name)
            } else {
                Self::find_word(|w| w.immediate, &self.immediate_word_lists, name)
            }
        })
    }

    pub fn words_mut(&mut self) -> RefMut<WordList<C>> {
        self.word_lists[0].borrow_mut()
    }
}
