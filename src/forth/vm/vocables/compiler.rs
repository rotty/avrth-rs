use byteorder::ByteOrder;
use failure::Error;

use forth::vm::vocables::Vocabulary;
use forth::vm::Cell;

pub fn load<C: Cell, B: ByteOrder>() -> Result<Vocabulary<'static, C, B>, Error> {
    let mut v = Vocabulary::new();
    primitives! {
        v,

        fn run_dp(vm, "dp") {
            let dp = vm.dp();
            vm.stack_push(dp);
        }
        fn run_i_store(vm, "i!") {
            let address = vm.stack_pop().unwrap();
            let w = vm.stack_pop().unwrap();
            vm.code_cell_set(address, w);
        }
        fn run_i_comma(vm, "i,") {
            let w = vm.stack_pop().unwrap();
            vm.code_push(w);
        }
        fn run_iallot(vm, "iallot") {
            let n = vm.stack_pop().unwrap();
            let dp = vm.dp();
            vm.set_dp(dp + n);
        }
    }
    v.define_forth_words(include_str!("compiler.fs"))?;
    Ok(v)
}

#[cfg(test)]
mod tests {
    use forth::vm::test_util::run_test;
    use forth::vm::{vocables, Cell, VocabularyLoader};

    use byteorder::LittleEndian;

    #[test]
    fn test_if() {
        let v: Vec<VocabularyLoader<u16, LittleEndian>> =
            vec![vocables::prim::load, vocables::compiler::load];
        assert_eq!(
            run_test(&v, &[u16::from_int(42)], "23 > if 42 else 0 then").unwrap(),
            vec![u16::from_int(42)]
        );
    }

    #[test]
    fn test_repeat() {
        let v: Vec<VocabularyLoader<u16, LittleEndian>> =
            vec![vocables::prim::load, vocables::compiler::load];
        let log2 = "2/ 0 begin over 0> while 1+ swap 2/ swap repeat nip";
        assert_eq!(run_test(&v, &[42], log2).unwrap(), vec![5]);
        assert_eq!(run_test(&v, &[567], log2).unwrap(), vec![9]);
    }

    #[test]
    fn test_qdo_loop() {
        let v: Vec<VocabularyLoader<u16, LittleEndian>> =
            vec![vocables::prim::load, vocables::compiler::load];
        let sum = "0 10 0 ?do i + loop";
        assert_eq!(run_test(&v, &[], sum).unwrap(), vec![45]);
    }
}
