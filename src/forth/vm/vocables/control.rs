///! Words that are "derived" from the words defined in the `prim`
///! module, and can be implemented in pure Forth based on these, and
///! the compilation words.
use byteorder::ByteOrder;

use crate::forth::vm::vocables::Vocabulary;
use crate::forth::vm::Cell;

pub fn load<C: Cell, B: ByteOrder>() -> anyhow::Result<Vocabulary<'static, C, B>> {
    let mut v = Vocabulary::new();
    v.define_forth_words(include_str!("control.fs"))?;
    Ok(v)
}
