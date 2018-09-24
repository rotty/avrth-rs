#![recursion_limit = "1024"]

extern crate avrth_asm;

extern crate combine;
extern crate env_logger;
extern crate failure;
#[macro_use]
extern crate structopt;

use avrth_asm::avr_asm::Assembler;
use avrth_asm::parser;

use combine::Parser;
use combine::stream::IteratorStream;
use combine::stream::buffered::BufferedStream;
use combine::stream::state::State;
use failure::Error;
use structopt::StructOpt;

use std::path::PathBuf;
use std::io::{BufReader, Read};
use std::fs::File;

#[derive(StructOpt, Debug)]
#[structopt(name = "avrth-asm", about = "AVR assembler")]
struct AsmArgs {
    #[structopt(parse(from_os_str))]
    files: Vec<PathBuf>,
}

fn main() -> Result<(), Error> {
    env_logger::init();

    let args = AsmArgs::from_args();
    for filename in args.files {
        let file = File::open(filename)?;
        let mut reader = BufReader::new(file);
        // TODO: Reading the whole file into memory is kinda suboptimal
        let mut input = String::new();
        reader.read_to_string(&mut input)?;
        let input = BufferedStream::new(State::new(IteratorStream::new(input.chars())), 128);
        let (commands, _rest): (Vec<_>, _) = parser::file().easy_parse(input)?;
        for command in commands {
            println!("{:?}", command);
        }
    }
    Ok(())
}
