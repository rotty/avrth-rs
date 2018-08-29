extern crate avrth;

extern crate byteorder;
extern crate failure;
#[macro_use]
extern crate structopt;

use std::fs::File;
use std::io;
use std::path::{Path, PathBuf};

use byteorder::LittleEndian;
use failure::{Error, ResultExt};
use structopt::StructOpt;

use avrth::avr_asm;
use avrth::forth::vm::{self, Vm, VmError};

#[derive(StructOpt, Debug)]
#[structopt(name = "avrth", about = "AVR Forth implementation")]
struct Avrth {
    #[structopt(subcommand)]
    command: Option<Command>,
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

fn make_standard_vm<R>(stdin: R) -> Vm<u16, LittleEndian>
where
    R: io::Read + 'static,
{
    Vm::<u16, LittleEndian>::new(vm::Options {
        ram_size: 2048,
        ram_start: 36,
        rstack_size: 40,
        flash_size: 32 * 1024,
        n_interrupts: 1,
        stdin: Box::new(stdin),
        assembler: Box::new(avr_asm::AvrAsm::new(avr_asm::Options {})),
    })
}

fn compile_program<O: io::Write, I: IntoIterator>(_output: O, input_files: I) -> Result<(), Error>
where
    I::Item: AsRef<Path>,
{
    let mut vm = make_standard_vm(io::stdin()).dictionary(vm::Dictionary::Target);
    for path in input_files.into_iter() {
        let file = vm.intern_file(File::open(path.as_ref())?);
        vm.stack_push(file);
        vm.run("include-file")
            .with_context(|_| format!("while compiling {}", path.as_ref().display()))?;
    }
    Ok(())
}

fn run_repl() {
    let mut vm = make_standard_vm(io::stdin());
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
}

fn run_program(file: &Path) -> Result<(), Error> {
    let mut vm = make_standard_vm(File::open(file)?);
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
    match Avrth::from_args().command {
        Some(Command::Compile { output, files }) => {
            compile_program(file_or_stdout(output)?, &files)
        }
        Some(Command::Tethered { tty }) => run_tethered(&tty),
        Some(Command::Run { file }) => run_program(&file),
        None => {
            run_repl();
            Ok(())
        }
    }
    // let args = App::new("avrth")
    //     .arg(Arg::with_name("tethered")
    //          .short("t")
    //          .long("tethered"))
    //     .arg(Arg::with_name("output")
    //          .short("o")
    //          .long("output")
    //          .takes_value(true)
    //          .value_name("FILE")
    //          .help("Write output to FILE"))
    //     .group(ArgGroup::with_name("mode").args(&["compile", "tethered"]))
    //     .get_matches();

    // if args.is_present("compile") {
    //     println!("Compile set");
    // } else {
    //     println!("Compile not set");
    // }
}
