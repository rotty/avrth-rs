use std::cell::{RefCell, RefMut};
use std::collections::{HashMap, VecDeque};
use std::fmt::Debug;
use std::fs;
use std::hash::Hash;
use std::io;
use std::marker::PhantomData;
use std::mem::size_of;
use std::ops::{Add, Sub};
use std::rc::Rc;

use byteorder::ByteOrder;
use failure::Fail;
use num_traits::int::PrimInt;
use num_traits::NumCast;

mod dict;

use self::dict::{Dict, Word, WordList};
use asm::Assembler;

#[derive(Debug, Copy, Clone)]
pub enum Dictionary {
    Target,
    Host,
}

pub trait Cell:
    PrimInt
    + From<u8>
    + Hash
    + From<<Self as Add>::Output>
    + From<<Self as Sub>::Output>
    + Into<usize>
    + Debug
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

type Interpreter<C, B> = fn(&mut Vm<C, B>, C, C) -> Result<(), VmError>;

pub struct Vm<C: Cell, B: ByteOrder> {
    current_dictionary: Dictionary,
    host_dict: Dict<C>,
    target_dict: Dict<C>,
    interpreters: HashMap<C, Interpreter<C, B>>,
    assembler: Box<dyn Assembler<C>>,
    ram: Vec<u8>,
    code: Vec<C>,
    sp: C,
    rsp: C,
    ip: C,
    files: Interns<C, File>,
    _byteorder: PhantomData<B>,
}

pub struct Options<C> {
    pub ram_size: C,
    pub rstack_size: C,
    pub flash_size: C,
    pub ram_start: C,
    pub n_interrupts: C,
    pub stdin: Box<dyn io::Read>,
    pub assembler: Box<dyn Assembler<C>>,
}

#[derive(Fail, Debug)]
pub enum VmError {
    #[fail(display = "Forth error {}", _0)]
    ForthError(isize),
    #[fail(display = "Undefined word '{}'", _0)]
    UndefinedWord(String),
}

impl VmError {
    fn forth_error<C: Cell>(errno: C) -> Self {
        // FIXME: proper conversion
        VmError::ForthError(NumCast::from(errno).unwrap())
    }
}

// impl Display for VmError {
//     fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
//         unimplemented!()
//     }
// }

impl<C: Cell, B: ByteOrder> Vm<C, B> {
    pub fn new(options: Options<C>) -> Self {
        let sp0 = options.ram_size - options.rstack_size + C::one();
        let rsp0 = options.ram_size - C::one();
        let forth_ivec = options.ram_start;
        let here0 = forth_ivec + options.n_interrupts;
        let mut ram = Vec::new();
        ram.resize(options.ram_size.into(), 0);
        let mut code = Vec::new();
        code.resize(options.flash_size.into(), C::zero()); // FIXME: use 0xFF for flash contents
        let target_words = Rc::new(RefCell::new(WordList::new()));
        let host_words = Rc::new(RefCell::new(WordList::new()));
        let mut files = Interns::new(1);
        files.set(0, File::Input(options.stdin));
        let mut vm = Vm {
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
            interpreters: HashMap::new(),
            assembler: options.assembler,
            ram: ram,
            code: code,
            files: files,
            sp: sp0,
            rsp: rsp0,
            ip: C::zero(), // zero is the trap address
            _byteorder: PhantomData,
        };
        let colon_pfa = vm.assembler.symbol("DO_COLON").unwrap();
        vm.register_interpreter(colon_pfa, Self::colon_interpreter);
        vm
    }

    fn colon_interpreter(&mut self, xt: C, _code: C) -> Result<(), VmError> {
        let ip = self.ip;
        self.rstack_push(ip);
        self.ip = Self::xt_to_pfa(xt);
        while self.ip > C::zero() {
            let xt = self.code_cell(self.ip);
            self.ip = self.ip + C::one();
            self.execute_xt(xt)?;
        }
        Ok(())
    }

    pub fn dictionary(mut self, d: Dictionary) -> Self {
        self.current_dictionary = d;
        self
    }

    fn register_interpreter(&mut self, pfa: C, interp: Interpreter<C, B>) {
        self.interpreters.insert(pfa, interp);
    }

    fn current_dict(&self) -> &Dict<C> {
        match self.current_dictionary {
            Dictionary::Target => &self.target_dict,
            Dictionary::Host => &self.host_dict,
        }
    }

    fn current_dict_mut(&mut self) -> &mut Dict<C> {
        match self.current_dictionary {
            Dictionary::Target => &mut self.target_dict,
            Dictionary::Host => &mut self.host_dict,
        }
    }

    fn words_mut(&mut self) -> RefMut<WordList<C>> {
        self.current_dict_mut().words_mut()
    }

    pub fn stack_push(&mut self, value: C) {
        self.sp = self.sp - C::size();
        self.stack_rset(0, value);
    }

    pub fn stack_pop(&mut self) -> Option<C> {
        unimplemented!()
    }

    fn stack_rset<O: Into<C>>(&mut self, offset: O, value: C) {
        let address = self.sp + offset.into() * C::size();
        self.ram_cell_set(address, value)
    }

    fn rstack_rset<O: Into<C>>(&mut self, offset: O, value: C) {
        let address = self.rsp + offset.into() * C::size();
        self.ram_cell_set(address, value)
    }

    fn rstack_push(&mut self, value: C) {
        self.rsp = self.rsp - C::size();
        self.rstack_rset(0, value);
    }

    fn ram_cell_set<T: Into<C>>(&mut self, address: T, value: C) {
        let address = address.into().into();
        value.write::<B>(&mut self.ram[address..]);
    }

    pub fn intern_file(&mut self, file: fs::File) -> C {
        self.files.add(File::Fs(file))
    }

    pub fn run(&mut self, start_name: &str) -> Result<(), VmError> {
        self.ip = C::zero();
        let xt = self
            .host_dict
            .get(start_name, false)
            .ok_or_else(|| VmError::UndefinedWord(start_name.into()))?
            .xt;
        let catch_xt = self
            .word_xt("catch")
            .ok_or_else(|| VmError::UndefinedWord("catch".into()))?;
        self.stack_push(xt);
        self.execute_xt(catch_xt)?;
        let error_number = self.stack_pop().ok_or_else(|| {
            // FIXME: context, "stack underflow" instead of code
            VmError::forth_error(6).into()
        })?;
        if error_number.is_zero() {
            Ok(())
        } else {
            Err(VmError::forth_error(error_number).into())
        }
    }

    fn code_cell(&self, address: C) -> C {
        self.code[address.into()]
    }

    fn execute_xt(&mut self, xt: C) -> Result<(), VmError> {
        // TODO: xtl-call
        let code = self.code_cell(xt);
        self.interpreters.get(&code).ok_or_else(|| -> VmError {
            // FIXME: context, "undefined interpreter" instead of (bogus) code
            VmError::forth_error(128).into()
        })?(self, xt, code)
    }

    fn word(&self, _name: &str) -> Option<&Word<C>> {
        unimplemented!()
    }

    fn word_xt(&self, name: &str) -> Option<C> {
        self.word(name).map(|w| w.xt)
    }

    fn xt_to_pfa(xt: C) -> C {
        xt + C::one()
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

    fn set<T: Into<C>>(&mut self, id: T, value: V) {
        self.table.insert(id.into(), value);
    }
}

enum File {
    Input(Box<dyn io::Read>),
    Output(Box<dyn io::Write>),
    Fs(fs::File),
}
