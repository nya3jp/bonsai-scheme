#[macro_use]
extern crate lazy_static;
extern crate regex;

use std::env;
use std::fs::File;
use std::io;
use std::io::prelude::*;

use environment::Env;

mod builtins;
mod data;
mod environment;
mod error;
mod forms;
mod parser;

fn main() {
    let args: Vec<String> = env::args().collect();

    let stdin = io::stdin();
    let mut file: Box<dyn Read> = if args.len() == 1 {
        Box::new(stdin.lock())
    } else if args.len() == 2 {
        Box::new(File::open(&args[1]).unwrap())
    } else {
        println!("Usage: minscheme file");
        return;
    };

    let mut code = String::new();
    let _ = file.read_to_string(&mut code).expect("Failed to read the file");
    let code = code;

    let exprs = parser::parse(&code).expect("Parse error");
    let env = Env::new_top_level();
    for expr in exprs.iter() {
        if let Err(ref msg) = Env::evaluate(&env, expr) {
            println!("ERROR: {}", msg);
            break;
        }
    }
}
