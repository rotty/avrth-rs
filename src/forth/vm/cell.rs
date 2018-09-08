use std::fmt::Debug;
use std::hash::Hash;
use std::mem::size_of;
use std::ops::{Add, BitAnd, Sub};

use byteorder::ByteOrder;
use num_traits::int::PrimInt;
use num_traits::{NumCast, WrappingAdd, WrappingSub};

pub trait Cell:
    PrimInt
    + WrappingAdd
    + WrappingSub
    + From<u8>
    + Hash
    + From<<Self as Add>::Output>
    + From<<Self as Sub>::Output>
    + Into<usize>
    + Debug
    + 'static
{
    fn read<B: ByteOrder>(buf: &[u8]) -> Self;
    fn write<B: ByteOrder>(self, buf: &mut [u8]);
    fn size() -> Self {
        NumCast::from(size_of::<Self>()).unwrap()
    }
    fn from_int(n: isize) -> Self {
        let mask: isize = NumCast::from(Self::max_value()).unwrap();
        NumCast::from(n.bitand(mask)).unwrap()
    }
    fn from_uint(n: usize) -> Self {
        let mask: usize = NumCast::from(Self::max_value()).unwrap();
        NumCast::from(n.bitand(mask)).unwrap()
    }
    fn to_int(self) -> isize;
    fn from_bool(b: bool) -> Self {
        if b {
            Self::max_value()
        } else {
            Self::zero()
        }
    }
}

impl Cell for u16 {
    fn read<B: ByteOrder>(buf: &[u8]) -> Self {
        B::read_u16(buf)
    }
    fn write<B: ByteOrder>(self, buf: &mut [u8]) {
        B::write_u16(buf, self);
    }
    fn to_int(self) -> isize {
        let n = self as isize;
        if self >= 0x8000 {
            -(0x1000 - n)
        } else {
            n
        }
    }
}
