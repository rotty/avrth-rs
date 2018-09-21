extern crate avrth;

extern crate byteorder;
extern crate env_logger;
extern crate failure;
extern crate structopt;

use std::fs::File;
use std::io;
use std::path::{Path, PathBuf};

use byteorder::LittleEndian;
use failure::{Error, ResultExt};
use structopt::StructOpt;

use avrth::forth::vm::{self, vocables, Vm, VmError};
use avrth::target::shim::ShimTarget;

#[derive(StructOpt, Debug)]
#[structopt(name = "avrth", about = "AVR Forth implementation")]
struct Avrth {
    #[structopt(subcommand)]
    command: Option<Command>,
    #[structopt(short = "L", number_of_values = 1, parse(from_os_str))]
    fpath: Vec<PathBuf>,
}

#[derive(StructOpt, Debug)]
enum Command {
    #[structopt(name = "compile")]
    /// Compile an ELF binary
    Compile {
        #[structopt(short = "o", long = "output", parse(from_os_str))]
        output: Option<PathBuf>,
        #[structopt(parse(from_os_str))]
        files: Vec<PathBuf>,
    },
    #[structopt(name = "tethered")]
    Tethered {
        #[structopt(parse(from_os_str))]
        tty: PathBuf,
    },
    #[structopt(name = "run")]
    Run {
        #[structopt(parse(from_os_str))]
        file: PathBuf,
    },
}

fn make_standard_vm<R, W, E>(
    stdin: R,
    stdout: W,
    stderr: E,
    fpath: Vec<Box<Path>>,
) -> Result<Vm<u16, LittleEndian>, Error>
where
    R: io::Read + 'static,
    W: io::Write + 'static,
    E: io::Write + 'static,
{
    Vm::<u16, LittleEndian>::new(vm::Options {
        ram_size: 2048,
        ram_start: 36,
        rstack_size: 40,
        flash_size: 32 * 1024,
        n_interrupts: 1,
        host_ram_size: 64 * 1024,
        host_code_size: 32 * 1024,
        stdin: Box::new(stdin),
        stdout: Box::new(stdout),
        stderr: Box::new(stderr),
        target: Box::new(ShimTarget::new()),
        fpath: fpath,
        layout: vec![(
            vm::Dictionary::Host,
            vec![
                vocables::prim::load,
                vocables::compiler::load,
                vocables::derived::load,
                vocables::compiler_high::load,
                vocables::store::load,
                vocables::control::load,
                vocables::io::load,
                vocables::repl::load,
                vocables::file::load,
            ],
        )],
    })
}

fn compile_program<O, I>(_output: O, input_files: I, fpath: Vec<PathBuf>) -> Result<(), Error>
where
    O: io::Write,
    I: IntoIterator,
    I::Item: AsRef<Path>,
{
    let mut vm = make_standard_vm(
        io::stdin(),
        io::stdout(),
        io::stderr(),
        fpath.into_iter().map(|p| p.into_boxed_path()).collect(),
    )?;
    vm.set_dictionary(vm::Dictionary::Target);
    for path in input_files.into_iter() {
        let file = vm.intern_file(File::open(path.as_ref())?);
        vm.stack_push(file);
        vm.run("include-file")
            .with_context(|_| format!("while compiling {}", path.as_ref().display()))?;
    }
    Ok(())
}

fn run_repl(fpath: Vec<PathBuf>) -> Result<(), Error> {
    let mut vm = make_standard_vm(
        io::stdin(),
        io::stdout(),
        io::stderr(),
        fpath.into_iter().map(|p| p.into_boxed_path()).collect(),
    )?;
    loop {
        // FIXME: deal with EOF, unexpected errors
        match vm.run("quit") {
            Err(error @ VmError::ForthError(_)) => {
                eprintln!("{}", error);
            }
            Err(error) => {
                eprintln!("{}", error);
                break;
            }
            Ok(()) => break,
        }
    }
    Ok(())
}

fn run_program(file: &Path, fpath: Vec<PathBuf>) -> Result<(), Error> {
    let mut vm = make_standard_vm(
        File::open(file)?,
        io::stdout(),
        io::stderr(),
        fpath.into_iter().map(|p| p.into_boxed_path()).collect(),
    )?;
    Ok(vm.run("quit")?)
}

fn run_tethered(_tty: &Path) -> Result<(), Error> {
    unimplemented!()
}

fn file_or_stdout<P>(file: Option<P>) -> Result<Box<dyn io::Write>, Error>
where
    P: AsRef<Path>,
{
    match file {
        Some(path) => Ok(File::open(path).map(Box::new)?),
        None => Ok(Box::new(io::stdout())),
    }
}

fn main() -> Result<(), Error> {
    env_logger::init();

    let avrth = Avrth::from_args();
    match avrth.command {
        Some(Command::Compile { output, files }) => {
            compile_program(file_or_stdout(output)?, &files, avrth.fpath)
        }
        Some(Command::Tethered { tty }) => run_tethered(&tty),
        Some(Command::Run { file }) => run_program(&file, avrth.fpath),
        None => {
            run_repl(avrth.fpath)?;
            Ok(())
        }
    }
}
