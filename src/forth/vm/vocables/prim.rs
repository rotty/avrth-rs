///! Primitive words, i.e. those that must defined both in Rust and
///! the target (assembly)
use std::thread;
use std::time;

use byteorder::ByteOrder;
use failure::Error;

use forth::vm::vocables::{SourceArena, Vocabulary};
use forth::vm::{Cell, Vm};

fn id<T>(x: T) -> T {
    x
}

macro_rules! binop {
    ($vm:ident, $method:ident, $convert_n2:tt) => {{
        // FIXME: unwrap
        let n2 = $vm.stack_pop().unwrap();
        let n1 = $vm.stack_pop().unwrap();
        $vm.stack_push(n1.$method($convert_n2(n2)));
        Ok(())
    }};
}

macro_rules! comparator {
    ($vm:ident, $method:ident, $converter:path) => {{
        // FIXME: unwrap
        let n2 = $vm.stack_pop().unwrap();
        let n1 = $vm.stack_pop().unwrap();
        $vm.stack_push(C::from_bool($converter(n1).$method(&$converter(n2))));
        Ok(())
    }};
}

pub fn load<C: Cell, B: ByteOrder>(_arena: &mut SourceArena) -> Result<Vocabulary<C, B>, Error> {
    let mut v = Vocabulary::new();
    primitives!{
        v,
        // Arithmetic
        fn run_plus(vm, "+") {
            binop!(vm, wrapping_add, &)
        }

        fn run_minus(vm, "-") {
            binop!(vm, wrapping_sub, &)
        }

        fn run_or(vm, "or") {
            binop!(vm, bitor, id)
        }
        fn run_one_plus(vm, "1+") {
            // FIXME: unwrap
            let a = vm.stack_pop().unwrap();
            vm.stack_push(a.wrapping_add(&C::one()));
            Ok(())
        }
        fn run_is_zero(vm, "0=") {
            // FIXME: unwrap
            let a = vm.stack_pop().unwrap();
            vm.stack_push(C::from_bool(a == C::zero()));
            Ok(())
        }

        fn run_invert(vm, "invert") {
            // FIXME: unwrap
            let a = vm.stack_pop().unwrap();
            vm.stack_push(a.bitxor(C::max_value()));
            Ok(())
        }

        fn run_gt(vm, ">") {
            comparator!(vm, gt, C::to_int)
        }

        fn run_lt(vm, "<") {
            comparator!(vm, lt, C::to_int)
        }
        
        fn run_eq(vm, "=") {
            comparator!(vm, lt, id)
        }

        fn run_m_star(vm, "m*") {
            let n2 = vm.stack_pop().unwrap().to_int();
            let n1 = vm.stack_pop().unwrap().to_int();
            vm.stack_dpush(n1 * n2);
            Ok(())
        }

        fn run_slash_mod(vm, "/mod") {
            let n2 = vm.stack_pop().unwrap().to_int();
            let n1 = vm.stack_pop().unwrap().to_int();
            vm.stack_push(C::from_int(n1 % n2));
            vm.stack_push(C::from_int(n1 / n2));
            Ok(())
        }

        // Stack manipulation
        fn run_drop(vm, "drop") {
            vm.stack_pop().unwrap();
            Ok(())
        }
        fn run_swap(vm, "swap") {
            let w1 = vm.stack_rget(1);
            let w2 = vm.stack_rget(0);
            vm.stack_rset(1, w2);
            vm.stack_rset(0, w1);
            Ok(())
        }
        fn run_over(vm, "over") {
            let a = vm.stack_rget(1);
            vm.stack_push(a);
            Ok(())
        }
        fn run_nip(vm, "nip") {
            let w = vm.stack_pop().unwrap();
            vm.stack_rset(0, w);
            Ok(())
        }

        // Return stack manipulation
        fn run_to_r(vm, ">r") {
            // FIXME: unwrap
            let a = vm.stack_pop().unwrap();
            vm.rstack_push(a);
            Ok(())
        }
        fn run_r_to(vm, "r>") {
            // FIXME: unwrap
            let a = vm.rstack_pop().unwrap();
            vm.stack_push(a);
            Ok(())
        }

        // Flow control and compiler support
        fn run_exit(vm, "exit") {
            // TODO: exit VM on return stack exhaustion?
            let return_ip = vm.rstack_pop().unwrap();
            vm.ip = return_ip;
            Ok(())
        }
        fn run_do_literal(vm, "(literal)") {
            let ip = vm.ip;
            let literal = vm.code_cell(ip);
            vm.stack_push(literal);
            vm.ip = ip + C::one();
            Ok(())
        }
        fn run_do_branch(vm, "(branch)") {
            vm.ip = vm.code_cell(vm.ip);
            Ok(())
        }
        fn run_do_0branch(vm, "(0branch)") {
            let w = vm.stack_pop().unwrap();
            if w == C::zero() {
                vm.ip = vm.code_cell(vm.ip);
            } else {
                vm.ip = vm.ip + C::one();
            }
            Ok(())
        }
        fn run_do_if_do(vm, "(?do)") {
            let count = vm.stack_pop().unwrap();
            let limit = vm.stack_pop().unwrap();
            let after_address = vm.code_cell(vm.ip);
            if count == limit {
                vm.ip = after_address;
            } else {
                vm.rstack_push(after_address);
                vm.rstack_push(limit);
                vm.rstack_push(count);
                vm.ip = vm.ip + C::one();
            }
            Ok(())
        }
        fn run_do_loop(vm, "(loop)") {
            let n = vm.rstack_rget(0).wrapping_add(&C::one());
            run_loop(vm, n);
            Ok(())
        }
        fn run_do_plus_loop(vm, "(+loop)") {
            let n = vm.stack_pop().unwrap();
            let i = vm.rstack_rget(0);
            run_loop(vm, i.wrapping_add(&n));
            Ok(())
        }
        fn run_do_do_does(vm, "((does>))") {
            vm.ip = vm.stack_pop().unwrap();
            Ok(())
        }

        fn run_1ms(_vm, "1ms") {
            thread::sleep(time::Duration::from_millis(1));
            Ok(())
        }
    }
    Ok(v)
}

fn run_loop<C: Cell, B: ByteOrder>(vm: &mut Vm<C, B>, n: C) {
    let limit = vm.stack_rget(1);
    let ip = vm.ip;
    if limit == n {
        vm.rstack_drop_n(C::from_int(3));
        vm.ip = ip + C::one();
    } else {
        vm.rstack_rset(0, n);
        let again_address = vm.code_cell(ip);
        vm.ip = again_address;
    }
}

#[cfg(test)]
mod tests {
    use forth::vm::test_util::run_test;
    use forth::vm::{vocables, Cell, VocabularyLoader};

    use byteorder::LittleEndian;

    #[test]
    fn arithmetic_ops() {
        let v: Vec<VocabularyLoader<u16, LittleEndian>> = vec![vocables::prim::load];
        assert_eq!(run_test(&v, &[1, 2], "+").unwrap(), vec![3]);
        assert_eq!(run_test(&v, &[u16::max_value(), 10], "+").unwrap(), vec![9]);
        assert_eq!(run_test(&v, &[1, 2], "-").unwrap(), vec![u16::from_int(-1)]);
    }
}
