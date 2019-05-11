#![recursion_limit = "1024"]

use avrth_asm::avr_asm::{self, Assembler};
use avrth_asm::parser::{self, Item};
use avrth_asm::lexer;

use combine::Parser;
use combine::stream::IteratorStream;
use combine::stream::buffered::BufferedStream;
use combine::stream::state::State;
use combine::stream::{easy, PointerOffset};
use failure::{format_err, Error, ResultExt};
use structopt::StructOpt;

use std::env;
use std::fmt;
use std::path::{Path, PathBuf};
use std::io::{self, BufReader, Read};
use std::fs::File;

#[derive(StructOpt, Debug)]
#[structopt(name = "avrth-asm", about = "AVR assembler")]
struct Cmd {
    #[structopt(parse(from_os_str))]
    files: Vec<PathBuf>,
    #[structopt(short = "I", number_of_values = 1, parse(from_os_str))]
    include_dirs: Vec<PathBuf>,
}

struct Asm {
    include_dirs: Vec<PathBuf>,
    current_dir: PathBuf,
    asm: Assembler,
}

impl Asm {
    fn new<I, P>(include_dirs: I) -> Result<Self, Error>
    where
        I: IntoIterator<Item = P>,
        P: AsRef<Path>
    {
        Ok(Asm {
            include_dirs: include_dirs.into_iter().map(|p| p.as_ref().to_path_buf()).collect(),
            current_dir: env::current_dir()?,
            asm: Assembler::new(),
        })
    }

    fn assemble_files<I, P>(&mut self, files: I) -> Result<(), Error>
    where
        I: IntoIterator<Item = P>,
        P: AsRef<Path>,
    {
        for filename in files {
            self.include_file(&filename)?;
        }
        Ok(())
    }

    fn include_file<P>(&mut self, filename: P) -> Result<(), Error>
    where
        P: AsRef<Path>
    {
        let filename = filename.as_ref();
        let file = self.open_file(true, filename)?;
        let mut reader = BufReader::new(file);
        // TODO: Reading the whole file into memory is kinda suboptimal
        let mut input : Vec<u8> = vec![];
        reader.read_to_end(&mut input)?;
        //let input = BufferedStream::new(State::new(IteratorStream::new()), 128);
        let (tokens, rest) : (Vec<lexer::Token>, _) = lexer::tokens().easy_parse(&input[..]).map_err(|e| {
            format_err!("{}:{}: lexer error", filename.display(), e.position)
        })?;
        if !rest.is_empty() {
            let token: String = String::from_utf8_lossy(rest).chars().take(5).collect();
            return Err(format_err!("{}: unexpected token: {}", filename.display(), token));
        }
        dbg!(&tokens);
        let (items, _): (Vec<parser::Item>, _) = parser::file().easy_parse(tokens.as_slice()).map_err(parse_error)?;
        for item in items {
            println!("{:?}", item);
            match item {
                Item::Directive(name, args) => self.handle_directive(&name, &args)?,
                Item::Label(name) => self.asm.define_symbol(&name, i32::from(self.asm.pc())),
                Item::Instruction(name, args) => {
                    self.asm.assemble(&name, &instruction_args(&args))?;
                }
            }
        }
        Ok(())
    }

    fn open_file<P>(&mut self, current: bool, filename: P) -> Result<File, Error>
    where
        P: AsRef<Path>
    {
        let filename = filename.as_ref();
        let mut dirs = if current {
            vec![&self.current_dir]
        } else {
            vec![]
        };
        dirs.extend(&self.include_dirs);
        for dir in &dirs {
            let path = dir.join(filename);
            match File::open(&path) {
                Ok(file) => {
                    self.current_dir = path.parent().unwrap().to_path_buf();
                    return Ok(file);
                },
                Err(_) => {},
            }
        }
        Err(io::Error::new(io::ErrorKind::NotFound,
                           format!("file {} not found in search path {}",
                                   filename.display(), SearchPath(&dirs)))
            .into())
    }

    fn handle_directive(&mut self, name: &str, args: &[parser::Expr]) -> Result<(), Error> {
        match name {
            "include" => {
                match args {
                    [parser::Expr::String(filename)] => {
                        self.include_file(filename)
                    }
                    _ => Err(format_err!(".include directive takes a single string literal"))
                }
            }
            _ => {
                Err(format_err!("unknown directive '{}'", name))
            }
        }
    }
}

fn main() -> Result<(), Error> {
    env_logger::init();

    let cmd = Cmd::from_args();
    let mut asm = Asm::new(cmd.include_dirs)?;
    asm.assemble_files(cmd.files)?;
    Ok(())
}

fn instruction_args(args: &[parser::Expr]) -> Vec<avr_asm::Expr> {
    unimplemented!()
}

struct SearchPath<'a>(&'a [&'a PathBuf]);

impl<'a> fmt::Display for SearchPath<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, path) in self.0.iter().enumerate() {
            write!(f, "{}", path.display())?;
            if i != self.0.len() - 1 {
                f.write_str(":")?;
            }
        }
        Ok(())
    }
}

type ParseError<'a> = easy::Errors<lexer::Token<'a>, &'a [lexer::Token<'a>], PointerOffset>;

fn parse_error(e: ParseError) -> Error {
    format_err!("parse error at {}", e.position)
}
