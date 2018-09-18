///! Primitive words, i.e. those that must defined both in Rust and
///! the target (assembly)
use std::convert;
use std::thread;
use std::time;

use byteorder::ByteOrder;
use failure::Error;

use forth::vm::vocables::{SourceArena, Vocabulary};
use forth::vm::{Cell, Prim, Vm, VmError};

fn id<T>(x: T) -> T {
    x
}

macro_rules! binop {
    ($vm:ident, $method:ident, $convert_n2:tt) => {{
        // FIXME: unwrap
        let n2 = $vm.stack_pop().unwrap();
        let n1 = $vm.stack_pop().unwrap();
        $vm.stack_push(n1.$method($convert_n2(n2)));
    }};
}

macro_rules! comparator {
    ($vm:ident, $method:ident, $converter:path) => {{
        // FIXME: unwrap
        let n2 = $vm.stack_pop().unwrap();
        let n1 = $vm.stack_pop().unwrap();
        $vm.stack_push(C::from_bool($converter(n1).$method(&$converter(n2))));
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
        fn run_and(vm, "and") {
            binop!(vm, bitand, id)
        }
        fn run_lshift(vm, "lshift") {
            binop!(vm, shl, (C::to_uint))
        }

        fn run_one_plus(vm, "1+") {
            // FIXME: unwrap
            let a = vm.stack_pop().unwrap();
            vm.stack_push(a.wrapping_add(&C::one()));
        }
        fn run_one_minus(vm, "1-") {
            let a = vm.stack_pop().unwrap();
            vm.stack_push(a.wrapping_sub(&C::one()));
        }
        fn run_is_zero(vm, "0=") {
            // FIXME: unwrap
            let a = vm.stack_pop().unwrap();
            vm.stack_push(C::from_bool(a == C::zero()));
        }
        fn run_zero_gt(vm, "0>") {
            let a = vm.stack_pop().unwrap();
            vm.stack_push(C::from_bool(a.to_int() > 0));
        }
        fn run_invert(vm, "invert") {
            // FIXME: unwrap
            let a = vm.stack_pop().unwrap();
            vm.stack_push(a.bitxor(C::max_value()));
        }

        fn run_gt(vm, ">") {
            comparator!(vm, gt, C::to_int)
        }
        fn run_lt(vm, "<") {
            comparator!(vm, lt, C::to_int)
        }
        fn run_eq(vm, "=") {
            comparator!(vm, eq, id)
        }
        fn run_neq(vm, "<>") {
            comparator!(vm, ne, id)
        }

        fn run_2_star(vm, "2*") {
            let n = vm.stack_pop().unwrap();
            vm.stack_push(n * C::from_uint(2));
        }
        fn run_2_slash(vm, "2/") {
            let n = vm.stack_pop().unwrap();
            vm.stack_push(n / C::from_uint(2));
        }
        fn run_rshift(vm, "rshift") {
            let n = vm.stack_pop().unwrap().to_int();
            let u1 = vm.stack_pop().unwrap().to_uint();
            vm.stack_push(C::from_uint(u1 >> n));
        }
        fn run_um_star(vm, "um*") {
            let u2 = vm.stack_pop().unwrap().into();
            let u1 = vm.stack_pop().unwrap().into();
            vm.stack_dpush(u1 * u2);
        }
        fn run_m_star(vm, "m*") {
            let n2 = vm.stack_pop().unwrap().to_int();
            let n1 = vm.stack_pop().unwrap().to_int();
            vm.stack_dpush((n1 * n2) as usize);
        }
        fn run_slash_mod(vm, "/mod") {
            let n2 = vm.stack_pop().unwrap().to_int();
            let n1 = vm.stack_pop().unwrap().to_int();
            vm.stack_push(C::from_int(n1 % n2));
            vm.stack_push(C::from_int(n1 / n2));
        }
        fn run_dplus(vm, "d+") {
            let n2 = vm.stack_dpop();
            let n1 = vm.stack_dpop();
            vm.stack_dpush(n1.wrapping_add(n2));
        }

        // Stack manipulation
        fn run_drop(vm, "drop") {
            vm.stack_pop().unwrap();
        }
        fn run_dup(vm, "dup") {
            let tos = vm.stack_rget(0);
            vm.stack_push(tos);
        }
        fn run_qdup(vm, "?dup") {
            let tos = vm.stack_rget(0);
            if tos != C::zero() {
                vm.stack_push(tos);
            }
        }
        fn run_swap(vm, "swap") {
            let w1 = vm.stack_rget(1);
            let w2 = vm.stack_rget(0);
            vm.stack_rset(1, w2);
            vm.stack_rset(0, w1);
        }
        fn run_over(vm, "over") {
            let a = vm.stack_rget(1);
            vm.stack_push(a);
        }
        fn run_rot(vm, "rot") {
            let w1 = vm.stack_rget(2);
            let w2 = vm.stack_rget(1);
            let w3 = vm.stack_rget(0);
            vm.stack_rset(2, w2);
            vm.stack_rset(1, w3);
            vm.stack_rset(0, w1);
        }
        fn run_nip(vm, "nip") {
            let w = vm.stack_pop().unwrap();
            vm.stack_rset(0, w);
        }

        // Return stack manipulation
        fn run_to_r(vm, ">r") {
            // FIXME: unwrap
            let a = vm.stack_pop().unwrap();
            vm.rstack_push(a);
        }
        fn run_r_to(vm, "r>") {
            // FIXME: unwrap
            let a = vm.rstack_pop().unwrap();
            vm.stack_push(a);
        }
        fn run_r_fetch(vm, "r@") {
            let a = vm.rstack_rget(0);
            vm.stack_push(a);
        }
        fn run_rdrop(vm, "rdrop") {
            vm.rstack_pop().unwrap();
        }
        fn run_i(vm, "i") {
            let w = vm.rstack_rget(0);
            vm.stack_push(w);
        }
        fn run_2_to_r(vm, "2>r") {
            // FIXME: unwrap
            let w2 = vm.stack_pop().unwrap();
            let w1 = vm.stack_pop().unwrap();
            vm.rstack_push(w1);
            vm.rstack_push(w2);
        }
        fn run_2_r_from(vm, "2r>") {
            // FIXME: unwrap
            let w2 = vm.rstack_pop().unwrap();
            let w1 = vm.rstack_pop().unwrap();
            vm.stack_push(w1);
            vm.stack_push(w2);
        }

        // VM register manipulation
        fn run_sp_store(vm, "sp!") {
            vm.sp = vm.stack_pop().unwrap();
        }
        fn run_sp_fetch(vm, "sp@") {
            let sp = vm.sp;
            vm.stack_push(sp);
        }
        fn run_rp_store(vm, "rp!") {
            vm.rsp = vm.stack_pop().unwrap();
        }
        fn run_rp_fetch(vm, "rp@") {
            let rsp = vm.rsp;
            vm.stack_push(rsp);
        }

        // Memory access
        fn run_fetch(vm, "@") {
            let address = vm.stack_pop().unwrap();
            let value = vm.ram_cell_get(address);
            vm.stack_push(value);
        }
        fn run_store(vm, "!") {
            let address = vm.stack_pop().unwrap();
            let value = vm.stack_pop().unwrap();
            vm.ram_cell_set(address, value);
        }
        fn run_plus_store(vm, "+!") {
            let address = vm.stack_pop().unwrap();
            let n = vm.stack_pop().unwrap();
            let value = vm.ram_cell_get(address);
            vm.ram_cell_set(address, value.wrapping_add(&n));
        }
        fn run_c_fetch(vm, "c@") {
            let address = vm.stack_pop().unwrap();
            let c = convert::From::from(vm.ram[address.into()]);
            vm.stack_push(c);
        }
        fn run_c_store(vm, "c!") {
            let address = vm.stack_pop().unwrap();
            let c = vm.stack_pop().unwrap();
            vm.ram[address.into()] = c.bitand(C::from_int(0xFF)).to_u8().unwrap();
        }
        fn run_i_fetch(vm, "i@") {
            let address = vm.stack_pop().unwrap();
            let value = vm.code_cell(address);
            vm.stack_push(value);
        }
        fn run_i_store(vm, "i!") {
            let address = vm.stack_pop().unwrap();
            let w = vm.stack_pop().unwrap();
            vm.code_cell_set(address, w);
        }

        // Flow control and compiler support
        fn run_exit(vm, "exit") {
            if vm.rstack_len() == C::zero() {
                return Ok(Prim::Exit)
            }
            let return_ip = vm.rstack_pop().unwrap();
            vm.ip = return_ip;
        }
        fn run_do_literal(vm, "(literal)") {
            let ip = vm.ip;
            let literal = vm.code_cell(ip);
            vm.stack_push(literal);
            vm.ip = ip + C::one();
        }
        fn run_do_branch(vm, "(branch)") {
            vm.ip = vm.code_cell(vm.ip);
        }
        fn run_do_0branch(vm, "(0branch)") {
            let w = vm.stack_pop().unwrap();
            if w == C::zero() {
                vm.ip = vm.code_cell(vm.ip);
            } else {
                vm.ip = vm.ip + C::one();
            }
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
        }
        fn run_do_loop(vm, "(loop)") {
            let n = vm.rstack_rget(0).wrapping_add(&C::one());
            run_loop(vm, n);
        }
        fn run_unloop(vm, "unloop") {
            vm.rstack_drop_n(C::from_int(3));
        }
        fn run_leave(vm, "leave") {
            vm.rstack_drop_n(C::from_int(2));
            vm.ip = vm.rstack_pop().unwrap();
        }
        fn run_do_plus_loop(vm, "(+loop)") {
            let n = vm.stack_pop().unwrap();
            let i = vm.rstack_rget(0);
            run_loop(vm, i.wrapping_add(&n));
        }
        fn run_do_do_does(vm, "((does>))") {
            vm.ip = vm.stack_pop().unwrap();
        }

        fn run_1ms(_vm, "1ms") {
            thread::sleep(time::Duration::from_millis(1));
        }
    }
    fn run_execute<C: Cell, B: ByteOrder>(vm: &mut Vm<C, B>) -> Result<Prim<C>, VmError> {
        let xt = vm.stack_pop().unwrap();
        Ok(Prim::Execute(xt))
    }
    v.define_primitive("execute", run_execute);
    Ok(v)
}

fn run_loop<C: Cell, B: ByteOrder>(vm: &mut Vm<C, B>, n: C) {
    let limit = vm.rstack_rget(1);
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
    extern crate env_logger;

    use forth::vm::test_util::run_test;
    use forth::vm::{vocables, Cell, VocabularyLoader};

    use byteorder::LittleEndian;

    #[test]
    fn arithmetic_ops() {
        let v: Vec<VocabularyLoader<u16, LittleEndian>> = vec![vocables::prim::load];
        assert_eq!(run_test(&v, &[1, 2], "+").unwrap(), vec![3]);
        assert_eq!(run_test(&v, &[u16::max_value(), 10], "+").unwrap(), vec![9]);
        assert_eq!(run_test(&v, &[1, 2], "-").unwrap(), vec![u16::from_int(-1)]);
        assert_eq!(run_test(&v, &[4], "1+").unwrap(), vec![5]);
    }

    #[test]
    fn comparisons() {
        let v: Vec<VocabularyLoader<u16, LittleEndian>> = vec![vocables::prim::load];
        assert_eq!(
            run_test(&v, &[1, 2], "<").unwrap(),
            vec![u16::from_bool(true)]
        );
        assert_eq!(
            run_test(&v, &[2, 1], "<").unwrap(),
            vec![u16::from_bool(false)]
        );
        assert_eq!(
            run_test(&v, &[u16::from_int(-42), 0], "<").unwrap(),
            vec![u16::from_bool(true)]
        );
        assert_eq!(
            run_test(&v, &[u16::from_int(-42), 0], "=").unwrap(),
            vec![u16::from_bool(false)]
        );
        assert_eq!(
            run_test(&v, &[42, 42], "=").unwrap(),
            vec![u16::from_bool(true)]
        );
    }

    #[test]
    fn swap() {
        let v: Vec<VocabularyLoader<u16, LittleEndian>> = vec![vocables::prim::load];
        assert_eq!(run_test(&v, &[1, 2], "swap").unwrap(), vec![2, 1]);
    }
}
