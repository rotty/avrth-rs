use std::cell::{Ref, RefCell, RefMut};
use std::collections::{BTreeMap, HashMap, VecDeque};
use std::fs;
use std::io::{self, Read, Write};
use std::marker::PhantomData;
use std::mem::size_of;
use std::path::Path;
use std::rc::Rc;
use std::str;

use byteorder::ByteOrder;
use log::trace;
use num_traits::NumCast;
use thiserror::Error;

pub mod cell;
mod dict;
pub mod vocables;

#[cfg(test)]
mod test_util;

pub use self::cell::Cell;
use self::dict::{Dict, Word, WordList};
use self::vocables::{Vocable, Vocabulary};
use crate::forth::reader::Token;
use crate::target::Target;

#[derive(Debug, Copy, Clone)]
pub enum Dictionary {
    Target,
    Host,
}

// FIXME: better name
pub enum Prim<C> {
    Continue,
    Exit,
    Execute(C),
}

type Primitive<C, B> = fn(&mut Vm<C, B>) -> Result<Prim<C>, VmError>;
type VocabularyLoader<C, B> = fn() -> anyhow::Result<Vocabulary<'static, C, B>>;

pub struct Vm<C: Cell, B: ByteOrder> {
    parameters: Parameters<C>,
    current_dictionary: Dictionary,
    current_word_name: Option<String>,
    immediate_mode: bool,
    host_dict: Dict<C>,
    target_dict: Dict<C>,
    primitives: HashMap<C, Primitive<C, B>>,
    target: Box<dyn Target<C>>,
    ram: Vec<u8>,
    code: Vec<C>,
    sp: C,
    rsp: C,
    ip: C,
    files: Interns<C, File>,
    string_tables: Interns<C, HashMap<String, C>>,
    paths: Interns<C, Vec<Box<Path>>>,
    source_stack: Vec<(C, Vec<u8>, C)>,
    current_source: C,
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
    pub stdout: Box<dyn io::Write>,
    pub stderr: Box<dyn io::Write>,
    pub target: Box<dyn Target<C>>,
    pub fpath: Vec<Box<Path>>,
    pub layout: Vec<(Dictionary, Vec<VocabularyLoader<C, B>>)>,
}

pub struct Parameters<C> {
    pub sp0: C,
    pub rp0: C,
    pub fpath: C,
}

#[derive(Error, Debug)]
pub enum VmError {
    #[error("Forth error {0}")]
    ForthError(isize),
    #[error("Undefined word '{0}'")]
    UndefinedWord(String),
}

impl VmError {
    fn forth_error<C: Cell>(errno: C) -> Self {
        // FIXME: proper conversion
        VmError::ForthError(NumCast::from(errno).unwrap())
    }
}

impl From<io::Error> for VmError {
    // TODO: keep info from io::Error
    fn from(_e: io::Error) -> Self {
        VmError::forth_error(37)
    }
}

// impl Display for VmError {
//     fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
//         unimplemented!()
//     }
// }

impl<C: Cell, B: ByteOrder> Vm<C, B> {
    pub fn new(options: Options<C, B>) -> anyhow::Result<Self> {
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
        let mut files = Interns::new(3);
        files.set(0, File::Input(options.stdin));
        files.set(1, File::Output(options.stdout));
        files.set(2, File::Output(options.stderr));
        let mut paths = Interns::new(0);
        let fpath = paths.add(options.fpath);
        let mut vm = Vm {
            parameters: Parameters {
                sp0,
                rp0: rsp0,
                fpath,
            },
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
                &[host_words, target_words],
                &[],
            ),
            primitives: HashMap::new(),
            target: options.target,
            ram,
            code,
            files,
            string_tables: Interns::new(0),
            paths,
            source_stack: vec![],
            current_source: C::zero(),
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
        //vm.register_interpreter(colon_pfa, Self::colon_interpreter);
        vm.words_mut().define("(:)", colon_pfa, false, false);
        vm.do_colon_address = colon_pfa;
        for (dict, vocabularies) in options.layout {
            vm.set_dictionary(dict);
            for loader in vocabularies {
                let vocabulary = loader()?;
                vm.compile_vocabulary(&vocabulary)?;
            }
        }
        Ok(vm)
    }

    fn execute_xt(&mut self, xt: C) -> Result<(), VmError> {
        trace!("inner_interpreter enter: ip={:?} xt={:?}", self.ip, xt);
        let mut xt = xt;
        let start_ip = self.ip;
        loop {
            let code = self.code_cell(xt);
            trace!(
                "inner_interpreter: ip={:?} xt={:?} code={:?} stack={:?} rstack={:?}",
                self.ip,
                xt,
                code,
                self.stack_contents(),
                self.rstack_contents(),
            );
            if code == self.do_colon_address {
                let ip = self.ip;
                self.rstack_push(ip);
                self.ip = Self::xt_to_pfa(xt);
            } else {
                let prim = *self.primitives.get(&code).ok_or_else(|| -> VmError {
                    // FIXME: context, "undefined interpreter" instead of (bogus) code
                    VmError::forth_error(128)
                })?;
                match prim(self) {
                    Err(e) => return Err(e),
                    Ok(Prim::Exit) => break,
                    Ok(Prim::Continue) => {}
                    Ok(Prim::Execute(exec_xt)) => {
                        xt = exec_xt;
                        continue;
                    }
                }
            }
            if self.ip == start_ip {
                break;
            }
            xt = self.do_next();
        }
        trace!("inner_interpreter exit: ip={:?} xt={:?}", self.ip, xt);
        Ok(())
    }

    fn do_next(&mut self) -> C {
        let ip = self.ip;
        let xt = self.code_cell(ip);
        self.ip = ip + C::one();
        xt
    }

    fn do_literal_xt(&self) -> Result<C, VmError> {
        // TODO: maybe memoize this
        self.word_xt("(literal)")
    }

    fn compile_vocabulary(&mut self, vocabulary: &Vocabulary<C, B>) -> Result<(), VmError> {
        for (name, vocable) in vocabulary.iter() {
            trace!(
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
                    self.register_primitive(pfa, *run);
                }
                Vocable::Forth { code, immediate } => {
                    self.compile_forth_vocable(name, code, *immediate)?;
                }
            }
        }
        Ok(())
    }

    fn compile_forth_vocable(
        &mut self,
        name: &str,
        code: &[Token],
        immediate: bool,
    ) -> Result<(), VmError> {
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
                Token::Ident("[']") => match state {
                    State::Interpret => unimplemented!(),
                    State::Compile => match tokens.next() {
                        Some(Token::Ident(name)) => {
                            let xt = self.word(name)?.xt;
                            let do_literal_xt = self.do_literal_xt()?;
                            self.code_push(do_literal_xt);
                            self.code_push(xt);
                        }
                        _ => unimplemented!(),
                    },
                },
                Token::Ident("[parameter]") => match tokens.next() {
                    Some(Token::Ident(name)) => {
                        let value = self.get_parameter(name).unwrap();
                        self.stack_push(value);
                    }
                    _ => unimplemented!(),
                },
                Token::Ident("[") => {
                    state = State::Interpret;
                }
                Token::Ident("]") => {
                    state = State::Compile;
                }
                Token::Ident(name) => {
                    let (immediate, xt) = {
                        let word = self.word(name)?;
                        (word.immediate, word.xt)
                    };
                    trace!(
                        "token={:?}, immediate={:?} state={:?}",
                        token,
                        immediate,
                        state
                    );
                    if immediate || state == State::Interpret {
                        self.execute_xt(xt)?;
                    } else {
                        self.code_push(xt);
                    }
                }
                Token::Int(n) => match state {
                    State::Compile => {
                        let do_literal_xt = self.do_literal_xt()?;
                        self.code_push(do_literal_xt);
                        self.code_push(C::from_int(*n));
                    }
                    State::Interpret => {
                        self.stack_push(C::from_int(*n));
                    }
                },
                Token::String(s) => {
                    let do_sliteral_xt = self.word_xt("(sliteral)")?;
                    self.code_push(do_sliteral_xt);
                    let len = s.len();
                    self.code_push(C::from_uint(len));
                    let dst = self.dp().into();
                    let mut acc = 0;
                    let cell_mask = size_of::<C>() - 1;
                    let mut cell_addr = dst;
                    for (idx, byte) in s.as_bytes().iter().copied().enumerate() {
                        let masked_idx = idx & cell_mask;
                        acc |= (byte as usize) << (8 * masked_idx);
                        if masked_idx == cell_mask {
                            self.code[cell_addr] = C::from_uint(acc);
                            cell_addr += 1;
                            acc = 0;
                        }
                    }
                    let n_complete_cells = len / size_of::<C>();
                    let remainder = len - (n_complete_cells * size_of::<C>());
                    if remainder > 0 {
                        self.code[cell_addr] = C::from_uint(acc);
                        cell_addr += 1;
                    }
                    self.set_dp(C::from_uint(cell_addr));
                }
                Token::Comment(_) => {}
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

    fn register_primitive(&mut self, pfa: C, interp: Primitive<C, B>) {
        self.primitives.insert(pfa, interp);
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
            .and_then(|name| self.words().get(name).copied())
    }

    fn set_current_word_xt(&mut self, xt: C) {
        let name = self.current_word_name.as_ref().unwrap().clone();
        self.words_mut().get_mut(&name).unwrap().xt = xt;
    }

    fn word_map(&self) -> BTreeMap<C, String> {
        self.words()
            .iter()
            .map(|(name, w)| (w.xt, name.clone()))
            .collect()
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

    pub fn stack_dpush(&mut self, value: usize) {
        self.stack_push(C::from_uint(value & 0xFFFF));
        self.stack_push(C::from_uint(value >> 16));
    }

    pub fn stack_dpop(&mut self) -> usize {
        let n2 = self.stack_pop().unwrap().into();
        let n1 = self.stack_pop().unwrap().into();
        n1 | (n2 << 16)
    }

    pub fn stack_pop_byteslice(&mut self) -> &[u8] {
        let u: usize = self.stack_pop().unwrap().into();
        let addr: usize = self.stack_pop().unwrap().into();
        &self.ram[addr..addr + u]
    }

    pub fn stack_pop_strslice(&mut self) -> &str {
        str::from_utf8(self.stack_pop_byteslice()).unwrap()
    }

    pub fn stack_pop_string(&mut self) -> String {
        String::from(self.stack_pop_strslice())
    }

    pub fn rstack_len(&self) -> C {
        (self.parameters.rp0 - self.rsp) / C::size()
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
        let mut contents = Vec::with_capacity(len);
        for i in (0..len).rev() {
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

    pub fn rstack_contents(&self) -> Vec<C> {
        let len = self.rstack_len().into();
        let mut contents = Vec::with_capacity(len);
        for i in (0..len).rev() {
            let offset: C = NumCast::from(i).unwrap();
            contents.push(self.rstack_rget(offset));
        }
        contents
    }

    fn ram_cell_set<T: Into<C>>(&mut self, address: T, value: C) {
        let address = address.into().into();
        value.write::<B>(&mut self.ram[address..]);
    }

    fn ram_cell_get<T: Into<C>>(&self, address: T) -> C {
        let address = address.into().into();
        C::read::<B>(&self.ram[address..])
    }

    fn get_parameter(&self, name: &str) -> Option<C> {
        match name {
            "sp0" => Some(self.parameters.sp0),
            "rp0" => Some(self.parameters.rp0),
            "fpath" => Some(self.parameters.fpath),
            _ => None,
        }
    }

    pub fn stdin_key(&mut self) -> Result<Option<u8>, VmError> {
        let mut buf = [0; 1];
        self.files
            .get_mut(self.current_source)
            .ok_or_else(|| VmError::forth_error(37))
            .and_then(|f| Ok(f.read(&mut buf)?))
            .map(|n_read| if n_read == 0 { None } else { Some(buf[0]) })
    }

    pub fn stdout_emit(&mut self, data: u8) -> Result<(), VmError> {
        self.stdout().and_then(|f| {
            let buf = [data; 1];
            f.write_all(&buf)?;
            f.flush()?;
            Ok(())
        })?;
        Ok(())
    }

    pub fn stdout(&mut self) -> Result<&mut File, VmError> {
        self.files
            .get_mut(1)
            .ok_or_else(|| VmError::forth_error(37))
    }

    pub fn intern_file(&mut self, file: fs::File) -> C {
        self.files.add(File::Fs(file))
    }

    pub fn intern_string_table(&mut self, table: HashMap<String, C>) -> C {
        self.string_tables.add(table)
    }

    pub fn source_push(&mut self, source_id: C, buffer: Vec<u8>, buffer_pos: C) {
        self.source_stack.push((source_id, buffer, buffer_pos));
    }

    pub fn source_pop(&mut self) -> (C, Vec<u8>, C) {
        self.source_stack.pop().unwrap()
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
            VmError::forth_error(6)
        })?;
        if error_number.is_zero() {
            Ok(())
        } else {
            Err(VmError::forth_error(error_number))
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
        trace!("code_push: {:?} {:?}", dp, code);
        self.code[dp.into()] = code;
        self.set_dp(dp + C::one());
    }

    fn word_get(&self, name: &str) -> Option<Word<C>> {
        self.current_dict().get(name, self.immediate_mode)
    }

    fn word(&self, name: &str) -> Result<Word<C>, VmError> {
        self.word_get(name)
            .ok_or_else(|| VmError::UndefinedWord(name.to_string()))
    }

    fn word_xt(&self, name: &str) -> Result<C, VmError> {
        self.word(name).map(|w| w.xt)
    }

    fn xt_to_pfa(xt: C) -> C {
        xt + C::one()
    }

    pub fn create(&mut self, name: &str) {
        let xt = self.dp();
        self.words_mut().define(name, xt, false, false);
        self.current_word_name = Some(name.into());
        // TODO define target symbol
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

    #[allow(dead_code)]
    fn get<T: Into<C>>(&self, id: T) -> Option<&V> {
        self.table.get(&id.into())
    }

    fn get_mut<T: Into<C>>(&mut self, id: T) -> Option<&mut V> {
        self.table.get_mut(&id.into())
    }

    fn remove<T: Into<C>>(&mut self, id: T) -> Option<V> {
        self.table.remove(&id.into())
    }
}

pub enum File {
    Input(Box<dyn io::Read>),
    Output(Box<dyn io::Write>),
    Fs(fs::File),
}

impl io::Read for File {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        match *self {
            File::Input(ref mut input) => input.read(buf),
            File::Output(_) => Err(io::Error::new(
                io::ErrorKind::Other,
                "attempt to read from output",
            )),
            File::Fs(ref mut file) => file.read(buf),
        }
    }
}

impl io::Write for File {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        match *self {
            File::Input(_) => Err(io::Error::new(
                io::ErrorKind::Other,
                "attempt to write to input",
            )),
            File::Output(ref mut output) => output.write(buf),
            File::Fs(ref mut file) => file.write(buf),
        }
    }
    fn flush(&mut self) -> io::Result<()> {
        match *self {
            File::Input(_) => Err(io::Error::new(
                io::ErrorKind::Other,
                "attempt to flush input",
            )),
            File::Output(ref mut output) => output.flush(),
            File::Fs(ref mut file) => file.flush(),
        }
    }
}
