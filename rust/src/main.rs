#[macro_use]
extern crate lazy_static;
extern crate regex;

use std::env;
use std::fmt;
use std::io;
use std::io::prelude::*;
use std::fs::File;
use std::rc::Rc;

use regex::Regex;

enum Value {
    Undef,
    Boolean(bool),
    Integer(i32),
    Symbol(String),
    Null,
    Pair(Rc<Value>, Rc<Value>),
}

impl Value {
    fn from_native_list(values: Vec<Rc<Value>>) -> Rc<Value> {
        let mut list_value = Rc::new(Value::Null);
        for value in values.into_iter().rev() {
            list_value = Rc::new(Value::Pair(value, list_value));
        }
        list_value
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Value::Undef => write!(f, "#undef"),
            &Value::Boolean(false) => write!(f, "#f"),
            &Value::Boolean(true) => write!(f, "#t"),
            &Value::Integer(i) => write!(f, "{}", i),
            &Value::Symbol(ref name) => write!(f, "{}", name),
            &Value::Null => write!(f, "()"),
            &Value::Pair(ref car, ref cdr) => write!(f, "({} . {})", &*car, &*cdr),
        }
    }
}

fn make_quote(value: Rc<Value>) -> Rc<Value> {
    Rc::new(Value::Pair(Rc::new(Value::Symbol("quote".to_string())), value))
}

fn parse_skip(code: &str) -> &str {
    lazy_static! {
        static ref SKIP_RE: Regex = Regex::new(r"^(\s+|;.*)+").unwrap();
    }

    match SKIP_RE.find(code) {
        None => code,
        Some(m) => &code[m.end()..],
    }
}

fn parse_value(code: &str) -> Result<(Rc<Value>, &str), String> {
    lazy_static! {
        static ref TOKEN_RE: Regex = Regex::new(r"^[^\s);]+").unwrap();
        static ref NUM_RE: Regex = Regex::new(r"^-?[0-9]+$").unwrap();
    }

    if code.starts_with("'") {
        let (value, next_code) = parse_value(&code[1..])?;
        return Ok((make_quote(value), next_code));
    }

    if code.starts_with("(") {
        let (values, next_code) = parse_list(&code[1..])?;
        let next_code = parse_skip(next_code);
        if !next_code.starts_with(")") {
            return Err("Parse error".to_string());
        }
        return Ok((Value::from_native_list(values), &next_code[1..]));
    }

    let m = TOKEN_RE.find(code).ok_or("Malformed token".to_string())?;
    let next_code = &code[m.end()..];
    let token = m.as_str();
    let value = if NUM_RE.find(token).is_some() {
        Value::Integer(token.parse::<i32>().ok().ok_or("Malformed integer")?)
    } else if token == "#t" {
        Value::Boolean(true)
    } else if token == "#f" {
        Value::Boolean(false)
    } else {
        Value::Symbol(token.to_string())
    };

    Ok((Rc::new(value), next_code))
}

fn parse_list(code: &str) -> Result<(Vec<Rc<Value>>, &str), String> {
    let mut values = Vec::new();
    let mut code = code;
    loop {
        code = parse_skip(code);
        if code.is_empty() || code.starts_with(")") {
            break;
        }
        let (value, next_code) = parse_value(code)?;
        values.push(value);
        code = next_code;
    }
    Ok((values, code))
}

fn parse(code: &str) -> Result<Vec<Rc<Value>>, String> {
    let (values, excess_code) = parse_list(code)?;
    if excess_code.is_empty() {
        Ok(values)
    } else {
        Err("Extra code".to_string())
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut file: Box<Read> = if args.len() == 1 {
        Box::new(io::stdin())
    } else if args.len() == 2 {
        Box::new(File::open(&args[1]).unwrap())
    } else {
        println!("usage: minilisp file");
        return;
    };

    let mut code = String::new();
    file.read_to_string(&mut code);
    let code = code;

    let exprs = parse(&code).expect("parse error");
    for expr in exprs {
        println!("{}", expr);
    }
}
