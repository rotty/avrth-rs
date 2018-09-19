///! Words that are "derived" from the words defined in the `prim`
///! module, and can be implemented in pure Forth based on these, and
///! the compilation words.
use byteorder::ByteOrder;
use failure::Error;

use forth::vm::vocables::Vocabulary;
use forth::vm::Cell;

pub fn load<C: Cell, B: ByteOrder>() -> Result<Vocabulary<'static, C, B>, Error> {
    let mut v = Vocabulary::new();
    v.define_forth_words(include_str!("control.fs"))?;
    Ok(v)
}
