///! Words that are "derived" from the words defined in the `prim`
///! module, and can be implemented in pure Forth based on these, and
///! the compilation words.
use byteorder::ByteOrder;

use crate::forth::vm::vocables::Vocabulary;
use crate::forth::vm::Cell;

pub fn load<C: Cell, B: ByteOrder>() -> anyhow::Result<Vocabulary<'static, C, B>> {
    let mut v = Vocabulary::new();
    v.define_forth_words(include_str!("derived.fs"))?;
    Ok(v)
}

#[cfg(test)]
mod tests {
    use crate::forth::vm::test_util::run_test;
    use crate::forth::vm::{vocables, Cell, VocabularyLoader};

    use byteorder::LittleEndian;

    #[test]
    fn arithmetic_ops() {
        let v: Vec<VocabularyLoader<u16, LittleEndian>> = vec![
            vocables::prim::load,
            vocables::compiler::load,
            vocables::derived::load,
        ];
        assert_eq!(
            run_test(&v, &[42], "negate").unwrap(),
            vec![u16::from_int(-42)]
        );
        assert_eq!(
            run_test(&v, &[2], "1 3 within").unwrap(),
            vec![u16::from_int(-1)]
        );
        assert_eq!(
            run_test(&v, &[2, u16::from_int(-42)], "min").unwrap(),
            vec![u16::from_int(-42)]
        );
    }
}
