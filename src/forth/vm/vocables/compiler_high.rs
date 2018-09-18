use byteorder::ByteOrder;
use failure::Error;

use forth::vm::vocables::{SourceArena, Vocabulary};
use forth::vm::Cell;

pub fn load<C: Cell, B: ByteOrder>(arena: &mut SourceArena) -> Result<Vocabulary<C, B>, Error> {
    let mut v = Vocabulary::new();
    primitives! {
        v,

        fn run_set_current(vm, "set-current") {
            let xt = vm.stack_pop().unwrap();
            vm.set_current_word_xt(xt);
        }
        fn get_current(vm, "get-current") {
            let xt = vm.current_word().unwrap().xt;
            vm.stack_push(xt);
        }
    }
    v.load_forth_words(arena, &["forth", "lib", "compiler-high.fs"])?;
    Ok(v)
}

#[cfg(test)]
mod tests {
    use forth::vm::test_util::run_test;
    use forth::vm::{vocables, VocabularyLoader};

    use byteorder::LittleEndian;

    #[test]
    fn load() {
        let v: Vec<VocabularyLoader<u16, LittleEndian>> = vec![
            vocables::prim::load,
            vocables::compiler::load,
            vocables::derived::load,
            vocables::compiler_high::load,
        ];
        assert_eq!(run_test(&v, &[], "[ 2 3 + ] literal").unwrap(), vec![5]);
    }
}
