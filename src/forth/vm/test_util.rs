use std::cell::RefCell;
use std::io::{self, Cursor};
use std::rc::Rc;

use byteorder::ByteOrder;
use failure::Error;

use forth::reader::Reader;
use forth::vm::vocables::Vocabulary;
use forth::vm::{Cell, Dictionary, Options, Vm, VocabularyLoader};
use target::shim::ShimTarget;

struct RefCursor(Rc<RefCell<Cursor<Vec<u8>>>>);

impl RefCursor {
    pub fn new() -> Self {
        RefCursor(Rc::new(RefCell::new(Cursor::new(Vec::new()))))
    }
    pub fn get(&self) -> Vec<u8> {
        self.0.borrow().get_ref().clone()
    }
}

impl io::Write for RefCursor {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0.borrow_mut().write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.0.borrow_mut().flush()
    }
}

impl Clone for RefCursor {
    fn clone(&self) -> Self {
        RefCursor(self.0.clone())
    }
}

fn make_vm<C: Cell, B: ByteOrder>(
    vocabularies: Vec<VocabularyLoader<C, B>>,
    stdin: &str,
    stdout: RefCursor,
    stderr: RefCursor,
) -> Result<Vm<C, B>, Error> {
    Vm::<C, B>::new(Options {
        ram_size: 2048,
        ram_start: 36,
        rstack_size: 40,
        flash_size: 32 * 1024,
        n_interrupts: 1,
        host_ram_size: 64 * 1024,
        host_code_size: 32 * 1024,
        stdin: Box::new(Cursor::new(stdin.as_bytes().to_owned())),
        stdout: Box::new(stdout),
        stderr: Box::new(stderr),
        target: Box::new(ShimTarget::new()),
        fpath: vec![],
        layout: vec![(Dictionary::Host, vocabularies)],
    })
}

pub fn run_io_test<C: Cell, B: ByteOrder>(
    vocabularies: &[VocabularyLoader<C, B>],
    stack: &[C],
    input: &str,
    code: &str,
) -> Result<(Vec<C>, String), Error> {
    let tokens: Vec<_> = Reader::new(code).tokens().collect();
    let stdout = RefCursor::new();
    let stderr = RefCursor::new();
    let mut vm = make_vm(vocabularies.to_vec(), input, stdout.clone(), stderr.clone())?;
    let test_vocabulary = {
        let mut v = Vocabulary::new();
        v.define_forth_word("test", false, tokens);
        v
    };
    vm.compile_vocabulary(&test_vocabulary)?;
    for entry in stack {
        vm.stack_push(*entry);
    }
    vm.execute_word("test")?;
    Ok((
        vm.stack_contents(),
        String::from_utf8(stdout.get()).expect("invalid UTF8 output"),
    ))
}

pub fn run_test<C: Cell, B: ByteOrder>(
    vocabularies: &[VocabularyLoader<C, B>],
    stack: &[C],
    code: &str,
) -> Result<Vec<C>, Error> {
    run_io_test(vocabularies, stack, "", code).map(|r| r.0)
}
