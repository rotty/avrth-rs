use std::cell::{RefCell, RefMut};
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
pub mod primitives;
mod vocables;

use self::dict::{Dict, Word, WordList};
use self::vocables::{SourceArena, Vocable, Vocabulary};
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
}

impl Cell for u16 {
    fn read<B: ByteOrder>(buf: &[u8]) -> Self {
        B::read_u16(buf)
    }
    fn write<B: ByteOrder>(self, buf: &mut [u8]) {
        B::write_u16(buf, self);
    }
}

type Interpreter<C, B> = fn(&mut Vm<C, B>, C) -> Result<(), VmError>;
type VocabularyLoader<C, B> = fn(&mut SourceArena) -> Result<Vocabulary<C, B>, Error>;

pub struct Vm<C: Cell, B: ByteOrder> {
    parameters: Parameters<C>,
    current_dictionary: Dictionary,
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
            ip: C::zero(), // zero is the trap address
            _byteorder: PhantomData,
        };
        let dp = vm.dp();
        let new_dp = vm.target.emit_startup_code(dp);
        vm.set_dp(new_dp);
        let colon_pfa = vm.target.symbol("DO_COLON").unwrap();
        vm.register_interpreter(colon_pfa, Self::colon_interpreter);
        for (dict, vocabularies) in options.layout {
            vm.set_dictionary(dict);
            for loader in vocabularies {
                let mut source_arena = SourceArena::new();
                let vocabulary = loader(&mut source_arena)?;
                vm.compile_vocabulary(&vocabulary);
            }
        }
        Ok(vm)
    }

    fn colon_interpreter(&mut self, xt: C) -> Result<(), VmError> {
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

    fn compile_vocabulary(&mut self, vocabulary: &Vocabulary<C, B>) {
        for (name, vocable) in vocabulary.iter() {
            match vocable {
                Vocable::Primitive { run } => {
                    let xt = self.dp();
                    self.words_mut().define(name, xt);
                    let pfa = Self::xt_to_pfa(xt);
                    self.code_push(pfa);
                    self.register_interpreter(pfa, *run);
                }
                Vocable::Forth { .. } => unimplemented!(),
            }
        }
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

    fn words_mut(&mut self) -> RefMut<WordList<C>> {
        self.current_dict_mut().words_mut()
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

    pub fn execute_word(&mut self, name: &str) -> Result<(), VmError> {
        let xt = self.host_xt(name)?;
        self.execute_xt(xt)
    }

    fn code_cell(&self, address: C) -> C {
        self.code[address.into()]
    }

    fn code_push(&mut self, code: C) {
        let dp = self.dp();
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

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use byteorder::LittleEndian;

    use super::*;
    use target::shim::ShimTarget;

    fn make_test_vm<C: Cell, B: ByteOrder>() -> Result<Vm<C, B>, Error> {
        Vm::<C, B>::new(Options {
            ram_size: 2048,
            ram_start: 36,
            rstack_size: 40,
            flash_size: 32 * 1024,
            n_interrupts: 1,
            host_ram_size: 64 * 1024,
            host_code_size: 32 * 1024,
            stdin: Box::new(Cursor::new(vec![])),
            target: Box::new(ShimTarget::new()),
            layout: vec![(Dictionary::Host, vec![primitives::load])],
        })
    }

    fn run_test<C: Cell, B: ByteOrder>(stack: &[C], word: &str) -> Result<Vec<C>, Error> {
        let mut vm = make_test_vm::<C, B>()?;
        for entry in stack.iter().rev() {
            vm.stack_push(*entry);
        }
        vm.execute_word(word)?;
        Ok(vm.stack_contents())
    }

    #[test]
    fn arithmetic_ops() {
        assert_eq!(
            run_test::<u16, LittleEndian>(&[1, 2], "+").unwrap(),
            vec![3]
        );
        assert_eq!(
            run_test::<u16, LittleEndian>(&[u16::max_value(), 10], "+").unwrap(),
            vec![9]
        );
        assert_eq!(
            run_test::<u16, LittleEndian>(&[1, 2], "-").unwrap(),
            vec![u16::from_int(-1)]
        );
    }
}
