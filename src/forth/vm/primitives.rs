use byteorder::ByteOrder;
use failure::Error;

use forth::vm::vocables::{SourceArena, Vocabulary};
use forth::vm::{Cell, Vm, VmError};

pub fn load<C: Cell, B: ByteOrder>(_arena: &mut SourceArena) -> Result<Vocabulary<C, B>, Error> {
    let mut v = Vocabulary::new();
    v.define_primitive("+", run_plus);
    v.define_primitive("-", run_minus);
    Ok(v)
}

fn run_plus<C: Cell, B: ByteOrder>(vm: &mut Vm<C, B>, _xt: C) -> Result<(), VmError> {
    // FIXME: unwrap
    let a = vm.stack_pop().unwrap();
    let b = vm.stack_pop().unwrap();
    vm.stack_push(a.wrapping_add(&b));
    Ok(())
}

fn run_minus<C: Cell, B: ByteOrder>(vm: &mut Vm<C, B>, _xt: C) -> Result<(), VmError> {
    // FIXME: unwrap
    let a = vm.stack_pop().unwrap();
    let b = vm.stack_pop().unwrap();
    vm.stack_push(a.wrapping_sub(&b));
    Ok(())
}
