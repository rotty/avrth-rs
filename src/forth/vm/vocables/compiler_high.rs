use byteorder::ByteOrder;

use crate::forth::vm::vocables::Vocabulary;
use crate::forth::vm::Cell;

pub fn load<C: Cell, B: ByteOrder>() -> anyhow::Result<Vocabulary<'static, C, B>> {
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
    v.define_forth_words(include_str!("compiler-high.fs"))?;
    Ok(v)
}

#[cfg(test)]
mod tests {
    use crate::forth::vm::test_util::run_test;
    use crate::forth::vm::{vocables, VocabularyLoader};

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
