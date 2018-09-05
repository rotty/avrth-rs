use std::io::Cursor;

use byteorder::ByteOrder;
use failure::Error;

use forth::vm::{Cell, Dictionary, Options, Vm, VocabularyLoader};
use target::shim::ShimTarget;

fn make_vm<C: Cell, B: ByteOrder>(vocabularies: Vec<VocabularyLoader<C, B>>) -> Result<Vm<C, B>, Error> {
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
        layout: vec![(Dictionary::Host, vocabularies)],
    })
}

pub fn run_test<C: Cell, B: ByteOrder>(vocabularies: &[VocabularyLoader<C, B>],
                                       stack: &[C],
                                       word: &str) -> Result<Vec<C>, Error> {
    let mut vm = make_vm(vocabularies.to_vec())?;
    for entry in stack {
        vm.stack_push(*entry);
    }
    vm.execute_word(word)?;
    Ok(vm.stack_contents())
}
