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

    #[test]
    fn load() {
        let v: Vec<VocabularyLoader<u16, LittleEndian>> = vec![
            vocables::prim::load,
            vocables::compiler::load,
            vocables::derived::load,
            vocables::compiler_high::load,
            vocables::store::load,
            vocables::io::load,
        ];
        assert_eq!(
            run_io_test(&v, &[], "", r#"s" Hello World!" itype"#).unwrap(),
            (vec![], "Hello World!".to_string())
        );
        assert_eq!(
            run_io_test(&v, &[], "", r#"s" Hello" itype"#).unwrap(),
            (vec![], "Hello".to_string())
        );
    }
}
