#![recursion_limit = "1024"]

use avrth_asm::avr_asm::{self, Assembler};
use avrth_asm::parser::{self, Item};

use combine::Parser;
use combine::stream::IteratorStream;
use combine::stream::buffered::BufferedStream;
use combine::stream::state::State;
use failure::{format_err, Error, ResultExt};
use structopt::StructOpt;

use std::path::{Path, PathBuf};
use std::io::{BufReader, Read};
use std::fs::File;

#[derive(StructOpt, Debug)]
#[structopt(name = "avrth-asm", about = "AVR assembler")]
struct Cmd {
    #[structopt(parse(from_os_str))]
    files: Vec<PathBuf>,
    #[structopt(short = "I", number_of_values = 1, parse(from_os_str))]
    include_dirs: Vec<PathBuf>,
}

struct State {
    options: Cmd,
    current_dir: PathBuf,
}

fn main() -> Result<(), Error> {
    env_logger::init();

    let cmdline = Cmd::from_args();
    let mut state = State {
        options: cmdline,
    };
    let mut assembler = Assembler::new();
    for filename in args.files {
        include_file(&cmdline, &mut assembler, &filename)?;
    }
    Ok(())
}

fn instruction_args(args: &[parser::Expr]) -> Vec<avr_asm::Expr> {
    unimplemented!()
}

fn open_file<P>(state: &State, filename: P) -> Result<File, Error>
where
    P: AsRef<Path>
{
    for dir in cmd.include_dirs {

    }
}

fn include_file<P>(cmd: &Cmd, assembler: &mut Assembler, filename: P) -> Result<(), Error>
where
    P: AsRef<Path>
{
    let filename = filename.as_ref();
    let file = File::open(filename).with_context(|e| format!("opening {} failed: {}", filename.display(), e))?;
    let mut reader = BufReader::new(file);
    // TODO: Reading the whole file into memory is kinda suboptimal
    let mut input = String::new();
    reader.read_to_string(&mut input)?;
    let input = BufferedStream::new(State::new(IteratorStream::new(input.chars())), 128);
    let (items, _rest): (Vec<_>, _) = parser::file().easy_parse(input).with_context(|e| {
        format!("error parsing {}", filename.display())
    })?;
    for item in items {
        println!("{:?}", item);
        match item {
            Item::Directive(name, args) => handle_directive(assembler, &name, &args)?,
            Item::Label(name) => assembler.define_symbol(&name, i32::from(assembler.pc())),
            Item::Instruction(name, args) => {
                assembler.assemble(&name, &instruction_args(&args))?;
            }
        }
    }
    Ok(())
}

fn handle_directive(assembler: &mut Assembler, name: &str, args: &[parser::Expr]) -> Result<(), Error> {
    match name {
        "include" => {
            match args {
                [parser::Expr::String(filename)] => {
                    include_file(assembler, filename)
                }
                _ => Err(format_err!(".include directive takes a single string literal"))
            }
        }
        _ => {
            Err(format_err!("unknown directive '{}'", name))
        }
    }
}
