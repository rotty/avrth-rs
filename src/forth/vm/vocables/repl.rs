use std::collections::BTreeMap;
use std::io;
use std::ops::Bound::{Excluded, Unbounded};
use std::str;

use byteorder::ByteOrder;
use failure::Error;

use forth::vm::vocables::{SourceArena, Vocabulary};
use forth::vm::{Cell, Vm};

pub fn load<C: Cell, B: ByteOrder>(arena: &mut SourceArena) -> Result<Vocabulary<C, B>, Error> {
    let mut v = Vocabulary::new();
    primitives! {
        v,

        //
        // Compiler support
        //
        fn run_do_create(vm, "(create)") {
            let name = vm.stack_pop_string();
            vm.create(&name);
        }

        // ( c-addr u -- [ addr 0 ] | [ xt [-1|1]] )
        fn run_find_name(vm, "find-name") {
            let n = vm.stack_pop().unwrap();
            let address = vm.stack_pop().unwrap();
            let start = address.into();
            let end = start + n.into();
            let word = {
                let name = str::from_utf8(&vm.ram[start..end]).unwrap();
                vm.word_get(name)
            };
            if let Some(word) = word {
                vm.stack_push(word.xt);
                vm.stack_push(if word.immediate { C::one() } else { C::max_value() });
            } else {
                vm.stack_push(address);
                vm.stack_push(C::zero());
            }
        }

        //
        // Development tools
        //
        fn run_xt_see(vm, "xt-see") {
            let xt = vm.stack_pop().unwrap();
            let do_colon_pfa = vm.word_xt("(:)").expect("word '(:)` not defined");
            let do_dodoes_pfa = vm.word_xt("((does>))").expect("word `((does>))' not defined");
            let pfa = vm.code_cell(xt);
            let words = vm.word_map();
            if let Some(_word) = words.get(&xt) {
                let next_xt = words.range((Excluded(xt), Unbounded)).map(|(xt, _)| *xt).next().unwrap_or(vm.dp());
                if pfa == do_colon_pfa || pfa == do_dodoes_pfa {
                    fmt_decompiled(io::stdout(), vm, &words, xt + C::one(), next_xt)?;
                } else {
                    println!("cannot decompile primitive, sorry");
                }
            } else {
                println!("unknown execution token {}", xt);
            }
        }
    }
    v.load_forth_words(arena, &["forth", "lib", "repl.fs"])?;
    Ok(v)
}

fn fmt_decompiled<C, B, W>(
    mut output: W,
    vm: &Vm<C, B>,
    words: &BTreeMap<C, String>,
    start: C,
    end: C,
) -> Result<(), io::Error>
where
    C: Cell,
    B: ByteOrder,
    W: io::Write,
{
    let do_literal_xt = vm.word_xt("(literal)").expect("word `(literal)' undefined");
    let start: usize = start.into();
    let end: usize = end.into();
    let mut address = start;
    while address < end {
        let xt = vm.code_cell(C::from_uint(address));
        if xt == do_literal_xt {
            write!(
                output,
                "{} {}\n",
                address,
                vm.code_cell(C::from_uint(address + 1))
            )?;
            address += 2;
        } else {
            if let Some(name) = words.get(&xt) {
                write!(output, "{} {}\n", address, name)?;
            } else {
                write!(output, "{} ?? \\ {}\n", address, xt)?;
            }
            address += 1;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use forth::vm::test_util::{run_io_test, run_test};
    use forth::vm::{vocables, VocabularyLoader};

    use byteorder::LittleEndian;

    fn vocables() -> Vec<VocabularyLoader<u16, LittleEndian>> {
        vec![
            vocables::prim::load,
            vocables::compiler::load,
            vocables::derived::load,
            vocables::compiler_high::load,
            vocables::store::load,
            vocables::control::load,
            vocables::io::load,
            vocables::repl::load,
        ]
    }

    #[test]
    fn test_base() {
        let v = vocables();
        assert_eq!(run_test(&v, &[], "base @").unwrap(), vec![10]);
        assert_eq!(run_test(&v, &[], "bin base @").unwrap(), vec![2]);
        assert_eq!(run_test(&v, &[], "hex base @").unwrap(), vec![16]);
    }

    // TODO: note that these do include input echo from `accept`

    #[test]
    fn test_parse() {
        let v = vocables();
        assert_eq!(
            run_io_test(&v, &['!' as u16], "foo bar! baz", "refill drop parse type").unwrap(),
            (vec![], "foo bar! baz\nfoo bar".to_string())
        );
    }

    #[test]
    fn test_parse_name() {
        let v = vocables();
        assert_eq!(
            run_io_test(
                &v,
                &[],
                "foo bar! baz",
                "refill drop 4 >in ! parse-name type"
            ).unwrap(),
            (vec![], "foo bar! baz\nbar!".to_string())
        );
    }

    #[test]
    fn test_interpret_numbers() {
        let v = vocables();
        assert_eq!(
            run_io_test(&v, &[], "1 2 3", "refill drop interpret").unwrap(),
            (vec![1, 2, 3], "1 2 3\n".to_string())
        );
    }

    #[test]
    fn test_interpret_plus() {
        let v = vocables();
        assert_eq!(
            run_io_test(&v, &[], "42 3 +", "refill drop interpret").unwrap(),
            (vec![45], "42 3 +\n".to_string())
        );
    }

    #[test]
    fn test_catch() {
        let v = vocables();
        let input = ": foo 42 throw ; ' foo catch\n";
        assert_eq!(
            run_io_test(&v, &[], input, "refill drop interpret").unwrap(),
            (vec![42], input.to_string())
        );
    }

    #[test]
    fn test_quit_eof() {
        let v = vocables();
        assert_eq!(
            run_io_test(&v, &[], "", "quit").unwrap(),
            (vec![], "\n> \n".into())
        );
    }
}
