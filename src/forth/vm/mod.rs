use std::cell::{Ref, RefCell, RefMut};
use std::collections::{HashMap, VecDeque};
use std::fmt::Debug;
use std::fs;
use std::hash::Hash;
use std::io;
use std::marker::PhantomData;
use std::mem::size_of;
use std::ops::{Add, BitAnd, Sub};
use std::rc::Rc;

use byteorder::ByteOrder;
use failure::Error;
use num_traits::int::PrimInt;
use num_traits::{NumCast, WrappingAdd, WrappingSub};

mod dict;
pub mod vocables;

#[cfg(test)]
mod test_util;

use self::dict::{Dict, Word, WordList};
use self::vocables::{SourceArena, Vocable, Vocabulary};
use forth::reader::Token;
use target::Target;

#[derive(Debug, Copy, Clone)]
pub enum Dictionary {
    Target,
    Host,
}

pub trait Cell:
    PrimInt
    + WrappingAdd
    + WrappingSub
    + From<u8>
    + Hash
    + From<<Self as Add>::Output>
    + From<<Self as Sub>::Output>
    + Into<usize>
    + Debug
    + 'static
{
    fn read<B: ByteOrder>(buf: &[u8]) -> Self;
    fn write<B: ByteOrder>(self, buf: &mut [u8]);
    fn size() -> Self {
        NumCast::from(size_of::<Self>()).unwrap()
    }
    fn from_int(n: isize) -> Self {
        let mask: isize = NumCast::from(Self::max_value()).unwrap();
        NumCast::from(n.bitand(mask)).unwrap()
    }
    fn to_int(self) -> isize;
    fn from_bool(b: bool) -> Self {
        if b {
            Self::max_value()
        } else {
            Self::zero()
        }
    }
}

impl Cell for u16 {
    fn read<B: ByteOrder>(buf: &[u8]) -> Self {
        B::read_u16(buf)
    }
    fn write<B: ByteOrder>(self, buf: &mut [u8]) {
        B::write_u16(buf, self);
    }
    fn to_int(self) -> isize {
        let n = self as isize;
        if self >= 0x8000 {
            -(0x1000 - n)
        } else {
            n
        }
    }
}

type Interpreter<C, B> = fn(&mut Vm<C, B>, C) -> Result<(), VmError>;
type VocabularyLoader<C, B> = fn(&mut SourceArena) -> Result<Vocabulary<C, B>, Error>;

pub struct Vm<C: Cell, B: ByteOrder> {
    parameters: Parameters<C>,
    current_dictionary: Dictionary,
    current_word_name: Option<String>,
    immediate_mode: bool,
    host_dict: Dict<C>,
    target_dict: Dict<C>,
    interpreters: HashMap<C, Interpreter<C, B>>,
    target: Box<dyn Target<C>>,
    ram: Vec<u8>,
    code: Vec<C>,
    sp: C,
    rsp: C,
    ip: C,
    files: Interns<C, File>,
    do_colon_address: C,
    _byteorder: PhantomData<B>,
}

pub struct Options<C: Cell, B: ByteOrder> {
    // TODO: split device and other options
    pub ram_size: usize,
    pub rstack_size: usize,
    pub flash_size: usize,
    pub ram_start: usize,
    pub n_interrupts: usize,
    pub host_ram_size: usize,
    pub host_code_size: usize,
    pub stdin: Box<dyn io::Read>,
    pub target: Box<dyn Target<C>>,
    pub layout: Vec<(Dictionary, Vec<VocabularyLoader<C, B>>)>,
}

pub struct Parameters<C> {
    pub sp0: C,
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
    pub fn new(options: Options<C, B>) -> Result<Self, Error> {
        let cast = |n| -> C { NumCast::from(n).unwrap() };
        let sp0 = cast(options.ram_size - (options.rstack_size * size_of::<C>()) - 1);
        let rsp0 = cast(options.ram_size - 1);
        let forth_ivec = options.ram_start;
        let here0 = cast(forth_ivec + options.n_interrupts);
        let mut ram = Vec::new();
        ram.resize(options.ram_size + options.host_ram_size, 0);
        let mut code = Vec::new();
        // FIXME: use 0xFF for flash contents
        code.resize(options.flash_size + options.host_code_size, C::zero());
        let target_words = Rc::new(RefCell::new(WordList::new()));
        let host_words = Rc::new(RefCell::new(WordList::new()));
        let mut files = Interns::new(1);
        files.set(0, File::Input(options.stdin));
        let mut vm = Vm {
            parameters: Parameters { sp0: sp0 },
            current_dictionary: Dictionary::Target,
            current_word_name: None,
            immediate_mode: false,
            target_dict: Dict::new(
                C::zero(),
                here0,
                &[target_words.clone(), host_words.clone()],
                &[host_words.clone()],
            ),
            host_dict: Dict::new(
                cast(options.flash_size),
                cast(options.ram_size),
                &[host_words.clone(), target_words.clone()],
                &[],
            ),
            interpreters: HashMap::new(),
            target: options.target,
            ram: ram,
            code: code,
            files: files,
            sp: sp0,
            rsp: rsp0,
            ip: C::zero(),               // zero is the trap address
            do_colon_address: C::zero(), // will be updated below
            _byteorder: PhantomData,
        };
        let dp = vm.dp();
        let new_dp = vm.target.emit_startup_code(dp);
        vm.set_dp(new_dp);
        let colon_pfa = vm.target.symbol("DO_COLON").unwrap();
        vm.register_interpreter(colon_pfa, Self::colon_interpreter);
        vm.words_mut().define("(:)", colon_pfa, false, false);
        vm.do_colon_address = colon_pfa;
        let mut source_arena = SourceArena::new();
        for (dict, vocabularies) in options.layout {
            vm.set_dictionary(dict);
            for loader in vocabularies {
                let vocabulary = loader(&mut source_arena)?;
                vm.compile_vocabulary(&vocabulary)?;
            }
        }
        Ok(vm)
    }

    fn colon_interpreter(&mut self, xt: C) -> Result<(), VmError> {
        println!("colon_interpreter: ip={:?} xt={:?}", self.ip, xt);
        let ip = self.ip;
        self.rstack_push(ip);
        self.ip = Self::xt_to_pfa(xt);
        while self.ip != C::zero() {
            let xt = self.code_cell(self.ip);
            self.ip = self.ip + C::one();
            self.execute_xt(xt)?;
        }
        Ok(())
    }

    fn do_literal_xt(&self) -> Result<C, VmError> {
        // TODO: maybe memoize this
        self.word_xt("(literal)")
    }

    fn compile_vocabulary(&mut self, vocabulary: &Vocabulary<C, B>) -> Result<(), VmError> {
        for (name, vocable) in vocabulary.iter() {
            println!(
                "compile: {} dp={:?}, vocable={:?}",
                name,
                self.dp(),
                vocable
            );
            match vocable {
                Vocable::Primitive { run } => {
                    let xt = self.dp();
                    self.words_mut().define(name, xt, false, false);
                    let pfa = Self::xt_to_pfa(xt);
                    self.code_push(pfa);
                    self.register_interpreter(pfa, *run);
                }
                Vocable::Forth { code, immediate } => {
                    self.compile_forth_vocable(name, code, *immediate)?;
                }
            }
        }
        Ok(())
    }

    fn compile_forth_vocable(&mut self, name: &str, code: &[Token], immediate: bool) -> Result<(), VmError> {
        #[derive(Copy, Clone, Eq, PartialEq, Debug)]
        enum State {
            Interpret,
            Compile,
        }
        let xt = self.dp();
        self.words_mut().define(name, xt, immediate, true);
        let do_colon = self.do_colon_address;
        let mut state = State::Compile;
        self.code_push(do_colon);
        let mut tokens = code.iter();
        while let Some(token) = tokens.next() {
            match token {
                Token::Ident("'") => {
                    match state {
                        State::Compile => {
                            let do_literal_xt = self.do_literal_xt()?;
                            self.code_push(do_literal_xt);
                        }
                        State::Interpret => {
                            match tokens.next() {
                                Some(Token::Ident(name)) => {
                                    let xt = self.word(name)?.xt;
                                    self.stack_push(xt);
                                }
                                _ => unimplemented!(),
                            }
                        }
                    }
                }
                Token::Ident("[") => {
                    state = State::Interpret;
                }
                Token::Ident("]") => {
                    state = State::Compile;
                }
                Token::Ident(name) => {
                    println!("token={:?}, immediate={:?}", token, immediate);
                    let (immediate, xt) = {
                        let word = self.word(name)?;
                        (word.immediate, word.xt)
                    };
                    if immediate || state == State::Interpret {
                        self.execute_xt(xt)?;
                    } else {
                        self.code_push(xt);
                    }
                }
                Token::Int(n) => {
                    let do_literal_xt = self.do_literal_xt()?;
                    self.code_push(do_literal_xt);
                    self.code_push(C::from_int(*n));
                }
                _ => unimplemented!(),
            }
        }
        let exit_xt = self.word_xt("exit").unwrap();
        self.code_push(exit_xt);
        self.words_mut().get_mut(name).unwrap().hidden = false;
        Ok(())
    }

    pub fn set_dictionary(&mut self, d: Dictionary) {
        self.current_dictionary = d;
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

    fn here(&self) -> C {
        self.current_dict().here()
    }

    fn set_here(&mut self, address: C) {
        self.current_dict_mut().set_here(address);
    }
    
    fn words(&self) -> Ref<WordList<C>> {
        self.current_dict().words()
    }
    
    fn words_mut(&mut self) -> RefMut<WordList<C>> {
        self.current_dict_mut().words_mut()
    }

    fn current_word(&self) -> Option<Word<C>> {
        self.current_word_name
            .as_ref()
            .and_then(|name| self.words().get(name).map(|w| w.clone()))
    }

    fn set_current_word_xt(&mut self, xt: C) {
        unimplemented!()
    }

    fn set_dp(&mut self, address: C) {
        self.current_dict_mut().set_dp(address)
    }

    fn dp(&self) -> C {
        self.current_dict().dp()
    }

    pub fn stack_len(&self) -> C {
        (self.parameters.sp0 - self.sp) / C::size()
    }

    pub fn stack_push(&mut self, value: C) {
        // TODO: overflow checking
        self.sp = self.sp - C::size();
        self.stack_rset(0, value);
    }

    pub fn stack_pop(&mut self) -> Option<C> {
        // TODO: underflow checking
        let w = self.stack_rget(0);
        self.sp = self.sp + C::size();
        Some(w)
    }

    pub fn stack_dpush(&mut self, value: isize) {
        self.stack_push(C::from_int(value));
        self.stack_push(C::from_int(value >> 16));
    }

    fn stack_rset<O: Into<C>>(&mut self, offset: O, value: C) {
        let address = self.sp + offset.into() * C::size();
        self.ram_cell_set(address, value)
    }

    fn stack_rget<O: Into<C>>(&self, offset: O) -> C {
        let address = self.sp + offset.into() * C::size();
        self.ram_cell_get(address)
    }

    pub fn stack_contents(&self) -> Vec<C> {
        let len = self.stack_len().into();
        let mut contents = Vec::with_capacity(len.into());
        for i in 0..len {
            let offset: C = NumCast::from(i).unwrap();
            contents.push(self.stack_rget(offset));
        }
        contents
    }

    fn rstack_rget<O: Into<C>>(&self, offset: O) -> C {
        let address = self.rsp + offset.into() * C::size();
        self.ram_cell_get(address)
    }

    fn rstack_rset<O: Into<C>>(&mut self, offset: O, value: C) {
        let address = self.rsp + offset.into() * C::size();
        self.ram_cell_set(address, value)
    }

    fn rstack_push(&mut self, value: C) {
        self.rsp = self.rsp - C::size();
        self.rstack_rset(0, value);
    }

    fn rstack_pop(&mut self) -> Option<C> {
        // TODO: underflow checking
        let w = self.rstack_rget(0);
        self.rsp = self.rsp + C::size();
        Some(w)
    }
    
    fn rstack_drop_n(&mut self, count: C) {
        self.rsp = self.rsp + count * C::size();
    }

    fn ram_cell_set<T: Into<C>>(&mut self, address: T, value: C) {
        let address = address.into().into();
        value.write::<B>(&mut self.ram[address..]);
    }

    fn ram_cell_get<T: Into<C>>(&self, address: T) -> C {
        let address = address.into().into();
        C::read::<B>(&self.ram[address..])
    }

    pub fn intern_file(&mut self, file: fs::File) -> C {
        self.files.add(File::Fs(file))
    }

    fn host_xt(&self, name: &str) -> Result<C, VmError> {
        Ok(self
            .host_dict
            .get(name, false)
            .ok_or_else(|| VmError::UndefinedWord(name.into()))?
            .xt)
    }

    pub fn run(&mut self, start_name: &str) -> Result<(), VmError> {
        self.ip = C::zero();
        let xt = self.host_xt(start_name)?;
        let catch_xt = self.word_xt("catch")?;
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

    pub fn execute_word(&mut self, name: &str) -> Result<(), VmError> {
        let xt = self.host_xt(name)?;
        self.execute_xt(xt)
    }

    fn code_cell(&self, address: C) -> C {
        self.code[address.into()]
    }

    fn code_cell_set(&mut self, address: C, value: C) {
        self.code[address.into()] = value;
    }

    fn code_push(&mut self, code: C) {
        let dp = self.dp();
        println!("code_push: {:?} {:?}", dp, code);
        self.code[dp.into()] = code;
        self.set_dp(dp + C::one());
    }

    fn execute_xt(&mut self, xt: C) -> Result<(), VmError> {
        // TODO: xtl-call
        let code = self.code_cell(xt);
        self.interpreters.get(&code).ok_or_else(|| -> VmError {
            // FIXME: context, "undefined interpreter" instead of (bogus) code
            VmError::forth_error(128).into()
        })?(self, xt)
    }

    fn word(&self, name: &str) -> Result<Word<C>, VmError> {
        self.current_dict().get(name, self.immediate_mode)
            .ok_or_else(|| VmError::UndefinedWord(name.to_string()))
    }

    fn word_xt(&self, name: &str) -> Result<C, VmError> {
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
