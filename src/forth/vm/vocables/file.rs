use std::cmp;
use std::collections::HashMap;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

use byteorder::ByteOrder;
use failure::Error;

use forth::vm::vocables::Vocabulary;
use forth::vm::{Cell, Prim};

pub fn load<C: Cell, B: ByteOrder>() -> Result<Vocabulary<'static, C, B>, Error> {
    let mut v = Vocabulary::new();
    primitives! {
        v,

        // ( -- tableid )
        fn run_create_string_table(vm, "create-string-table") {
            let id = vm.intern_string_table(HashMap::new());
            vm.stack_push(id);
        }

        // ( c-addr u tableid -- -1 | 0 )
        fn run_string_table_contains(vm, "string-table-contains?") {
            let table_id = vm.stack_pop().unwrap();
            let key = vm.stack_pop_string();
            let contained = vm.string_tables.get(table_id).unwrap().contains_key(&key);
            vm.stack_push(C::from_bool(contained));
        }

        // ( u1 c-addr u2 tableid -- )
        fn run_string_table_insert(vm, "string-table-insert") {
            let table_id = vm.stack_pop().unwrap();
            let key = vm.stack_pop_string();
            let value = vm.stack_pop().unwrap();
            vm.string_tables.get_mut(table_id).unwrap().insert(key, value);
        }

        // ( c-addr u wfam -- wfileid wior )
        fn run_open_file(vm, "open-file") {
            let fam = vm.stack_pop().unwrap();
            let path = vm.stack_pop_string();
            match open_file(&path, fam) {
                Ok(file) => {
                    let fileid = vm.intern_file(file);
                    vm.stack_push(fileid);
                    vm.stack_push(C::zero());
                }
                Err(e) => {
                    vm.stack_push(C::max_value());
                    vm.stack_push(io_result(e.kind()));
                }
            }
        }

        // ( wfileid -- )
        fn run_close_file(vm, "close-file") {
            let file_id = vm.stack_pop().unwrap();
            match vm.files.remove(file_id) {
                Some(_) => vm.stack_push(C::zero()),
                None => vm.stack_push(C::from_int(-1)), // TODO: proper error code
            }
        }

        // ( fileid addr size pos -- )
        fn run_comma_push_source(vm, ",push-source") {
            let buffer_pos = vm.stack_pop().unwrap();
            let buffer = vm.stack_pop_byteslice().to_vec();
            let source_id = vm.stack_pop().unwrap();
            let current_source_id = vm.current_source;
            vm.source_push(current_source_id, buffer, buffer_pos);
            vm.current_source = source_id;
        }

        // ( addr size -- >in )
        fn run_comma_pop_source(vm, ",pop-source") {
            let buffer_size = vm.stack_pop().unwrap();
            let buffer_addr = vm.stack_pop().unwrap().into();
            let (source_id, buffer, buffer_pos) = vm.source_pop();
            let len = buffer.len();
            assert!(len <= buffer_size.into());
            vm.ram[buffer_addr..buffer_addr + len].copy_from_slice(&buffer[..len]);
            vm.current_source = source_id;
            vm.stack_push(buffer_pos);
        }

        // ( addr1 u1 path-addr addr2 u2 -- wfileid addr2 u3 0 | ior )
        fn run_comma_open_path_file(vm, ",open-path-file") {
            let buffer_size = vm.stack_pop().unwrap();
            let buffer_addr = vm.stack_pop().unwrap();
            let path_addr = vm.stack_pop().unwrap();
            let filename = vm.stack_pop_string();
            let path = vm.paths.get(path_addr).unwrap().clone();
            for pathname in search_path(path, &filename) {
                match open_file(&pathname, 1) { // TODO: fam abstraction
                    Ok(file) => {
                        let pathname_utf8 = pathname.to_str().unwrap().as_bytes();
                        // TODO: handling for too-long names
                        let n_bytes = cmp::min(buffer_size, C::from_uint(pathname_utf8.len()));
                        let len: usize = n_bytes.into();
                        let dst: usize = buffer_addr.into();
                        vm.ram[dst..dst + len].copy_from_slice(&pathname_utf8[..len]);
                        let file_id = vm.intern_file(file);
                        vm.stack_push(file_id);
                        vm.stack_push(buffer_addr);
                        vm.stack_push(n_bytes);
                        vm.stack_push(C::zero());
                        return Ok(Prim::Continue)
                    }
                    Err(error) => {
                        if error.kind() != io::ErrorKind::NotFound {
                            vm.stack_push(io_result(error.kind()));
                            return Ok(Prim::Continue);
                        }
                    }
                }
            }
            vm.stack_push(io_result(io::ErrorKind::NotFound));
        }
    }
    v.define_forth_words(include_str!("file.fs"))?;
    Ok(v)
}

struct SearchPathIter<P, S> {
    path: P,
    iter: S,
}

impl<P, S> Iterator for SearchPathIter<P, S>
where
    P: AsRef<Path>,
    S: Iterator,
    S::Item: AsRef<Path>,
{
    type Item = PathBuf;
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|base| base.as_ref().join(&self.path))
    }
}

fn search_path<P, S>(path: S, filename: P) -> SearchPathIter<P, S::IntoIter>
where
    P: AsRef<Path>,
    S: IntoIterator,
    S::Item: AsRef<Path>,
{
    SearchPathIter {
        path: filename,
        iter: path.into_iter(),
    }
}

fn open_file<C: Cell, P: AsRef<Path>>(path: P, fam: C) -> Result<fs::File, io::Error> {
    let fam = fam.to_uint();
    let mut mode = fs::OpenOptions::new();
    if (fam & 1) != 0 {
        mode.read(true);
    }
    if (fam & 2) != 0 {
        mode.write(true);
        mode.create(true);
    }
    mode.open(path)
}

fn io_result<C: Cell>(kind: io::ErrorKind) -> C {
    use std::io::ErrorKind::*;

    match kind {
        NotFound => C::from_int(-38),
        PermissionDenied => C::from_int(-37),
        // TODO: handle more kinds
        _ => C::from_int(-1),
    }
}

#[cfg(test)]
mod tests {
    extern crate env_logger;

    use forth::vm::test_util::{run_io_test, run_test};
    use forth::vm::{vocables, Cell, VocabularyLoader};

    use byteorder::LittleEndian;

    fn vocables() -> Vec<VocabularyLoader<u16, LittleEndian>> {
        vec![
            vocables::prim::load,
            vocables::compiler::load,
            vocables::derived::load,
            vocables::compiler_high::load,
            vocables::store::load,
            vocables::io::load,
            vocables::control::load,
            vocables::repl::load,
            vocables::file::load,
        ]
    }

    #[test]
    fn load() {
        let v = vocables();
        assert_eq!(
            run_io_test(&v, &[], "", "").unwrap(),
            (vec![], "".to_string())
        );
    }

    #[test]
    fn test_string_tables() {
        let v = vocables();
        assert_eq!(
            run_test(
                &v,
                &[],
                r#"create-string-table s" foo" rot string-table-contains?"#
            ).unwrap(),
            vec![u16::from_bool(false)]
        );
        assert_eq!(
            run_test(
                &v,
                &[],
                concat!(
                    r#"create-string-table dup s" foo" rot string-table-add"#,
                    r#" s" foo" rot string-table-contains?"#
                )
            ).unwrap(),
            vec![u16::from_bool(true)]
        );
    }
}
