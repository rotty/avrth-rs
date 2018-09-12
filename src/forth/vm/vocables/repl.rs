use std::str;

use byteorder::ByteOrder;
use failure::Error;

use forth::vm::vocables::{SourceArena, Vocabulary};
use forth::vm::Cell;

pub fn load<C: Cell, B: ByteOrder>(arena: &mut SourceArena) -> Result<Vocabulary<C, B>, Error> {
    let mut v = Vocabulary::new();
    primitives! {
        v,

        fn run_do_create(vm, "(create)") {
            let name = vm.stack_pop_string();
            vm.create(&name);
            Ok(())
        }

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
    use forth::vm::test_util::{run_io_test, run_test};
    use forth::vm::{vocables, VocabularyLoader};

    use byteorder::LittleEndian;

    fn vocables() -> Vec<VocabularyLoader<u16, LittleEndian>> {
        vec![
            vocables::prim::load,
            vocables::compiler::load,
            vocables::derived::load,
            vocables::compiler_high::load,
            vocables::store::load,
            vocables::control::load,
            vocables::io::load,
            vocables::repl::load,
        ]
    }

    #[test]
    fn test_base() {
        let v = vocables();
        assert_eq!(run_test(&v, &[], "base @").unwrap(), vec![10]);
        assert_eq!(run_test(&v, &[], "bin base @").unwrap(), vec![2]);
        assert_eq!(run_test(&v, &[], "hex base @").unwrap(), vec![16]);
    }

    // TODO: note that these do include input echo from `accept`

    #[test]
    fn test_parse() {
        let v = vocables();
        assert_eq!(
            run_io_test(&v, &['!' as u16], "foo bar! baz", "refill drop parse type").unwrap(),
            (vec![], "foo bar! baz\nfoo bar".to_string())
        );
    }

    #[test]
    fn test_parse_name() {
        let v = vocables();
        assert_eq!(
            run_io_test(
                &v,
                &[],
                "foo bar! baz",
                "refill drop 4 >in ! parse-name type"
            ).unwrap(),
            (vec![], "foo bar! baz\nbar!".to_string())
        );
    }

    #[test]
    fn test_interpret_numbers() {
        let v = vocables();
        assert_eq!(
            run_io_test(&v, &[], "1 2 3", "refill drop interpret").unwrap(),
            (vec![1, 2, 3], "1 2 3\n".to_string())
        );
    }

    #[test]
    fn test_interpret_plus() {
        let v = vocables();
        assert_eq!(
            run_io_test(&v, &[], "42 3 +", "refill drop interpret").unwrap(),
            (vec![45], "42 3 +\n".to_string())
        );
    }
}
