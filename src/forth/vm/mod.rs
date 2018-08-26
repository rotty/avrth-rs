use std::cell::RefCell;
use std::collections::{HashMap, VecDeque};
use std::fs;
use std::hash::Hash;
use std::marker::PhantomData;
use std::mem::size_of;
use std::ops::{Add, Sub};
use std::rc::Rc;

use byteorder::ByteOrder;
use failure::Error;
use num_traits::int::PrimInt;
use num_traits::NumCast;

#[derive(Debug, Copy, Clone)]
pub enum Dictionary {
    Target,
    Host,
}

mod dict;

use self::dict::{Dict, WordList};

pub trait Cell:
    PrimInt + From<u8> + Hash + From<<Self as Add>::Output> + From<<Self as Sub>::Output> + Into<usize>
{
    fn read<B: ByteOrder>(buf: &[u8]) -> Self;
    fn write<B: ByteOrder>(self, buf: &mut [u8]);
    fn size() -> Self {
        NumCast::from(size_of::<Self>()).unwrap()
    }
}

impl Cell for u16 {
    fn read<B: ByteOrder>(buf: &[u8]) -> Self {
        B::read_u16(buf)
    }
    fn write<B: ByteOrder>(self, buf: &mut [u8]) {
        B::write_u16(buf, self);
    }
}

pub struct Vm<C: Cell, B: ByteOrder> {
    current_dictionary: Dictionary,
    host_dict: Dict<C>,
    target_dict: Dict<C>,
    ram: Vec<u8>,
    sp: C,
    ports: Interns<C, fs::File>,
    _byteorder: PhantomData<B>,
}

pub struct Options<C> {
    pub ram_size: C,
    pub rstack_size: C,
    pub flash_size: C,
    pub ram_start: C,
    pub n_interrupts: C,
}

impl<C: Cell, B: ByteOrder> Vm<C, B> {
    pub fn new(options: &Options<C>) -> Self {
        let sp0 = options.ram_size - options.rstack_size + C::one();
        let forth_ivec = options.ram_start;
        let here0 = forth_ivec + options.n_interrupts;
        let mut ram = Vec::new();
        ram.resize(options.ram_size.into(), 0.into());
        let target_words = Rc::new(RefCell::new(WordList::new()));
        let host_words = Rc::new(RefCell::new(WordList::new()));
        Vm {
            current_dictionary: Dictionary::Target,
            target_dict: Dict::new(
                C::zero(),
                here0,
                &[target_words.clone(), host_words.clone()],
                &[host_words.clone()],
            ),
            host_dict: Dict::new(
                options.flash_size,
                options.ram_size,
                &[host_words.clone(), target_words.clone()],
                &[],
            ),
            ram: ram,
            ports: Interns::new(1),
            sp: sp0,
            _byteorder: PhantomData,
        }
    }
    pub fn dictionary(mut self, d: Dictionary) -> Self {
        self.current_dictionary = d;
        self
    }

    fn current_dict(&self) -> &Dict<C> {
        match self.current_dictionary {
            Dictionary::Target => &self.target_dict,
            Dictionary::Host => &self.host_dict,
        }
    }

    pub fn stack_push(&mut self, value: C) {
        self.sp = self.sp - C::size();
        self.stack_rset(0, value);
    }

    fn stack_rset<O: Into<C>>(&mut self, offset: O, value: C) {
        let address = self.sp + offset.into() * C::size();
        self.ram_cell_set(address, value)
    }

    fn ram_cell_set<T: Into<C>>(&mut self, address: T, value: C) {
        let address = address.into().into();
        value.write::<B>(&mut self.ram[address..]);
    }

    pub fn intern_file(&mut self, file: fs::File) -> C {
        self.ports.add(file)
    }

    pub fn run(&mut self, word: &str) -> Result<(), Error> {
        unimplemented!()
    }
}

struct Interns<C, V> {
    free_ids: VecDeque<C>,
    next_id: C,
    table: HashMap<C, V>,
}

impl<C: Cell, V> Interns<C, V> {
    fn new<T: Into<C>>(first_id: T) -> Self {
        Interns {
            free_ids: VecDeque::new(),
            next_id: first_id.into(),
            table: HashMap::new(),
        }
    }

    fn add(&mut self, value: V) -> C {
        let id = self.free_ids.pop_front().unwrap_or_else(|| {
            let id = self.next_id;
            self.next_id = id + C::one();
            id
        });
        let old = self.table.insert(id, value);
        assert!(old.is_none());
        id
    }
}
