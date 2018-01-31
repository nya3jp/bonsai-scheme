#[macro_use]
extern crate lazy_static;
extern crate regex;

use std::env;
use std::fs::File;
use std::io;
use std::io::prelude::*;

mod builtins;
mod data;
mod environment;
mod parser;

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut file: Box<Read> = if args.len() == 1 {
        Box::new(io::stdin())
    } else if args.len() == 2 {
        Box::new(File::open(&args[1]).unwrap())
    } else {
        println!("Usage: minilisp file");
        return;
    };

    let mut code = String::new();
    let _ = file.read_to_string(&mut code).expect("Failed to read the file");
    let code = code;

    let exprs = parser::parse(&code).expect("Parse error");
    let env = environment::Env::new_top_level();
    for expr in exprs {
        if let Err(ref msg) = env.evaluate(expr) {
            println!("ERROR: {}", msg);
            break;
        }
    }
}
