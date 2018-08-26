extern crate avrth;

extern crate byteorder;
extern crate failure;
extern crate structopt;

use std::fs::File;
use std::io;
use std::path::{Path, PathBuf};

use byteorder::LittleEndian;
use failure::{Error, ResultExt};
use structopt::StructOpt;

use avrth::forth::vm;

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

fn compile_program<O: io::Write, I: IntoIterator>(_output: O, input_files: I) -> Result<(), Error>
where
    I::Item: AsRef<Path>,
{
    let mut vm = vm::Vm::<u16, LittleEndian>::new(&vm::Options {
        ram_size: 2048,
        ram_start: 36,
        rstack_size: 40,
        flash_size: 32 * 1024,
        n_interrupts: 1,
    }).dictionary(vm::Dictionary::Target);
    for path in input_files.into_iter() {
        let file = vm.intern_file(File::open(path.as_ref())?);
        vm.stack_push(file);
        vm.run("include-file")
            .with_context(|_| format!("while compiling {}", path.as_ref().display()))?;
    }
    Ok(())
}

fn run_input<R: io::Read>(_input: R) -> Result<(), Error> {
    unimplemented!()
}

fn run_program(file: &Path) -> Result<(), Error> {
    run_input(File::open(file)?)
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
        None => run_input(io::stdin()),
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
