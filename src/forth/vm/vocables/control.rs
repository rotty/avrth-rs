///! Words that are "derived" from the words defined in the `prim`
///! module, and can be implemented in pure Forth based on these, and
///! the compilation words.
use byteorder::ByteOrder;
use failure::Error;

use forth::reader::Reader;
use forth::vm::vocables::{SourceArena, Vocabulary};
use forth::vm::Cell;

pub fn load<C: Cell, B: ByteOrder>(arena: &mut SourceArena) -> Result<Vocabulary<C, B>, Error> {
    let mut reader = Reader::new(arena.load(&["forth", "lib", "control.fs"])?);
    let mut v = Vocabulary::new();
    v.define_forth_words(&mut reader)?;
    Ok(v)
}
