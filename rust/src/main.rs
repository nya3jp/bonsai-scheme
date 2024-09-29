#[macro_use]
extern crate lazy_static;
extern crate regex;

use std::{
    env,
    fs::File,
    io::{self, prelude::*},
    process::ExitCode,
};

use anyhow::Result;

use environment::Env;

mod builtins;
mod data;
mod environment;
mod forms;
mod parser;

fn main() -> Result<ExitCode> {
    let args: Vec<String> = env::args().collect();

    let stdin = io::stdin();
    let file: Box<dyn Read> = if args.len() == 1 {
        Box::new(stdin.lock())
    } else if args.len() == 2 {
        Box::new(File::open(&args[1]).unwrap())
    } else {
        println!("Usage: minscheme file");
        return Ok(ExitCode::FAILURE);
    };

    let code = std::io::read_to_string(file)?;
    let exprs = parser::parse(&code)?;
    let env = Env::new_top_level();
    for expr in exprs {
        if let Err(ref msg) = env.evaluate(&expr) {
            println!("ERROR: {}", msg);
            break;
        }
    }

    Ok(ExitCode::SUCCESS)
}
