use std::rc::Rc;

use data::Value;
use regex::Regex;

fn make_quote(value: Rc<Value>) -> Rc<Value> {
    Value::from_native_list(
        &[Rc::new(Value::Symbol("quote".to_string())), value])
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
        return Ok((Value::from_native_list(values.as_slice()), &next_code[1..]));
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

pub fn parse(code: &str) -> Result<Vec<Rc<Value>>, String> {
    let (values, excess_code) = parse_list(code)?;
    if excess_code.is_empty() {
        Ok(values)
    } else {
        Err("Extra code".to_string())
    }
}