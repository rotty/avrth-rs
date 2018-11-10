#![recursion_limit = "1024"]

#[macro_use]
extern crate combine;
#[macro_use]
extern crate failure;

pub mod avr_asm;
pub mod lexer;
pub mod parser;
