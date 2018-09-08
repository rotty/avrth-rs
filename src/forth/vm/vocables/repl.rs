use std::str;

use byteorder::ByteOrder;
use failure::Error;

use forth::vm::vocables::{SourceArena, Vocabulary};
use forth::vm::Cell;

pub fn load<C: Cell, B: ByteOrder>(arena: &mut SourceArena) -> Result<Vocabulary<C, B>, Error> {
    let mut v = Vocabulary::new();
    primitives! {
        v,

        fn run_find_name(vm, "find-name") {
            let n = vm.stack_pop().unwrap();
            let address = vm.stack_pop().unwrap();
            let start = address.into();
            let end = start + n.into();
            let word = {
                let name = str::from_utf8(&vm.ram[start..end]).unwrap();
                vm.word_get(name)
            };
            if let Some(word) = word {
                vm.stack_push(word.xt);
                vm.stack_push(if word.immediate { C::one() } else { C::max_value() });
            } else {
                vm.stack_push(address);
                vm.stack_push(C::zero());
            }
            Ok(())
        }
    }
    v.load_forth_words(arena, &["forth", "lib", "repl.fs"])?;
    Ok(v)
}

#[cfg(test)]
mod tests {
    use forth::vm::test_util::run_test;
    use forth::vm::{vocables, Cell, VocabularyLoader};

    use byteorder::LittleEndian;

    #[test]
    fn load() {
        let v: Vec<VocabularyLoader<u16, LittleEndian>> = vec![
            vocables::prim::load,
            vocables::compiler::load,
            vocables::derived::load,
            vocables::compiler_high::load,
            vocables::store::load,
            vocables::control::load,
            vocables::io::load,
            vocables::repl::load,
        ];
        // FIXME: do some real tests here
        assert_eq!(
            run_test(&v, &[42], "negate").unwrap(),
            vec![u16::from_int(-42)]
        );
    }
}
