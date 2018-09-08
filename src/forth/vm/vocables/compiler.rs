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

#[cfg(test)]
mod tests {
    use forth::vm::test_util::run_test;
    use forth::vm::{vocables, Cell, VocabularyLoader};

    use byteorder::LittleEndian;

    #[test]
    fn test_if() {
        let v: Vec<VocabularyLoader<u16, LittleEndian>> =
            vec![vocables::prim::load, vocables::compiler::load];
        assert_eq!(
            run_test(&v, &[u16::from_int(42)], "23 > if 42 else 0 then").unwrap(),
            vec![u16::from_int(42)]
        );
    }
}
