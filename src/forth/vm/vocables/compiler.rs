use byteorder::ByteOrder;
use failure::Error;

use forth::vm::vocables::{SourceArena, Vocabulary};
use forth::vm::Cell;

pub fn load<C: Cell, B: ByteOrder>(arena: &mut SourceArena) -> Result<Vocabulary<C, B>, Error> {
    let mut v = Vocabulary::new();
    primitives! {
        v,

        fn run_dp(vm, "dp") {
            let dp = vm.dp();
            vm.stack_push(dp);
            Ok(())
        }
        fn run_i_store(vm, "i!") {
            let address = vm.stack_pop().unwrap();
            let w = vm.stack_pop().unwrap();
            vm.code_cell_set(address, w);
            Ok(())
        }
        fn run_i_comma(vm, "i,") {
            let w = vm.stack_pop().unwrap();
            vm.code_push(w);
            Ok(())
        }
    }
    v.load_forth_words(arena, &["forth", "lib", "compiler.fs"])?;
    Ok(v)
}
