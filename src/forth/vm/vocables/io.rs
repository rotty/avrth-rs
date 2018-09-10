use std::io::Write;

use byteorder::ByteOrder;
use failure::Error;

use forth::vm::vocables::{SourceArena, Vocabulary};
use forth::vm::Cell;

pub fn load<C: Cell, B: ByteOrder>(arena: &mut SourceArena) -> Result<Vocabulary<C, B>, Error> {
    let mut v = Vocabulary::new();
    primitives! {
        v,
        fn run_key(vm, "key") {
            let key = match vm.stdin_key()? {
                None => C::max_value(),
                Some(key) => C::from_int(key as isize),
            };
            vm.stack_push(key);
            Ok(())
        }
        fn run_emit(vm, "emit") {
            let code = vm.stack_pop().unwrap().to_int() as u8;
            vm.stdout_emit(code)?;
            Ok(())
        }
        fn run_dot(vm, ".") {
            let u = vm.stack_pop().unwrap();
            write!(vm.stdout()?, "{}", u)?;
            Ok(())
        }
    }
    v.load_forth_words(arena, &["forth", "lib", "io.fs"])?;
    Ok(v)
}

#[cfg(test)]
mod tests {
    use forth::vm::test_util::run_io_test;
    use forth::vm::{vocables, VocabularyLoader};

    use byteorder::LittleEndian;

    fn vocables() -> Vec<VocabularyLoader<u16, LittleEndian>> {
        vec![
            vocables::prim::load,
            vocables::compiler::load,
            vocables::derived::load,
            vocables::compiler_high::load,
            vocables::store::load,
            vocables::io::load,
        ]
    }

    #[test]
    fn test_itype() {
        let v = vocables();
        assert_eq!(
            run_io_test(&v, &[], "", r#"s" Hello World!" itype"#).unwrap(),
            (vec![], "Hello World!".to_string())
        );
        assert_eq!(
            run_io_test(&v, &[], "", r#"s" Hello" itype"#).unwrap(),
            (vec![], "Hello".to_string())
        );
    }

    #[test]
    fn test_hash_tib() {
        let v = vocables();
        assert_eq!(
            run_io_test(&v, &[], "", "#tib @ 42 #tib ! #tib @").unwrap(),
            (vec![0, 42], "".to_string())
        );
    }

    #[test]
    fn test_key() {
        let v = vocables();
        let mut expected: Vec<_> = "hi!\n".chars().map(|c| c as u16).collect();
        expected.push(u16::max_value()); // EOF
        assert_eq!(
            run_io_test(&v, &[], "hi!\n", "key key key key key").unwrap(),
            (expected, "".to_string())
        );
    }

    #[test]
    fn test_accept() {
        let v = vocables();
        assert_eq!(
            run_io_test(&v, &[], "hello\n", "tib tibsize accept tib swap type").unwrap(),
            (vec![], "hello".to_string())
        );
    }

    #[test]
    fn test_refill() {
        let v = vocables();
        assert_eq!(
            run_io_test(&v, &[], "", "refill drop source type").unwrap(),
            (vec![], "hello\nhello".to_string())
        );
    }
}
