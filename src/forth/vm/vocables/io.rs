use byteorder::ByteOrder;
use failure::Error;

use forth::vm::vocables::{SourceArena, Vocabulary};
use forth::vm::Cell;

pub fn load<C: Cell, B: ByteOrder>(arena: &mut SourceArena) -> Result<Vocabulary<C, B>, Error> {
    let mut v = Vocabulary::new();
    primitives! {
        v,
    }
    v.load_forth_words(arena, &["forth", "lib", "io.fs"])?;
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
            vocables::io::load,
        ];
        // FIXME: do some real tests here
        assert_eq!(
            run_test(&v, &[42], "negate").unwrap(),
            vec![u16::from_int(-42)]
        );
    }
}
