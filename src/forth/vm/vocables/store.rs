use byteorder::ByteOrder;
use failure::Error;

use forth::vm::vocables::{SourceArena, Vocabulary};
use forth::vm::Cell;

pub fn load<C: Cell, B: ByteOrder>(_arena: &mut SourceArena) -> Result<Vocabulary<C, B>, Error> {
    let mut v = Vocabulary::new();
    primitives! {
        v,

        fn run_here(vm, "here") {
            let here = vm.here();
            vm.stack_push(here);
        }

        fn run_allot(vm, "allot") {
            let n = vm.stack_pop().unwrap();
            let here = vm.here();
            vm.set_here(here + n);
        }

        fn run_store(vm, ",") {
            let here = vm.here();
            let value = vm.stack_pop().unwrap();
            vm.ram_cell_set(here, value);
            vm.set_here(here + C::size());
        }

        fn run_cmove(vm, "cmove>") {
            let n: usize = vm.stack_pop().unwrap().into();
            let addr_to: usize = vm.stack_pop().unwrap().into();
            let addr_from: usize = vm.stack_pop().unwrap().into();
            let ram = &mut vm.ram[..];
            for i in 1..n + 1 {
                let offset = n - i;
                ram[addr_to + offset] = ram[addr_from + offset];
            }
        }
    }
    Ok(v)
}
