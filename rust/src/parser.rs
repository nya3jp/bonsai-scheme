use std::rc::Rc;

use anyhow::{anyhow, ensure, Result};

use crate::{data::Value, regex::Regex};

fn make_quote(value: Value) -> Value {
    Value::from_native_list(&[Value::Symbol(Rc::new("quote".to_owned())), value])
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

fn parse_value(code: &str) -> Result<(Value, &str)> {
    lazy_static! {
        static ref TOKEN_RE: Regex = Regex::new(r"^[^\s);]+").unwrap();
        static ref NUM_RE: Regex = Regex::new(r"^-?[0-9]+$").unwrap();
    }

    if let Some(rest_code) = code.strip_prefix('\'') {
        let (value, next_code) = parse_value(rest_code)?;
        return Ok((make_quote(value), next_code));
    }

    if let Some(rest_code) = code.strip_prefix('(') {
        let (values, next_code) = parse_list(rest_code)?;
        let next_code = parse_skip(next_code);
        ensure!(next_code.starts_with(')'), "Parse error");
        return Ok((Value::from_native_list(&values), &next_code[1..]));
    }

    let m = TOKEN_RE.find(code).ok_or(anyhow!("Malformed token"))?;
    let next_code = &code[m.end()..];
    let token = m.as_str();
    let value = if NUM_RE.find(token).is_some() {
        Value::Integer(
            token
                .parse::<i32>()
                .ok()
                .ok_or(anyhow!("Malformed integer"))?,
        )
    } else if token == "#t" {
        Value::Boolean(true)
    } else if token == "#f" {
        Value::Boolean(false)
    } else {
        Value::Symbol(Rc::new(token.to_owned()))
    };

    Ok((value, next_code))
}

fn parse_list(code: &str) -> Result<(Vec<Value>, &str)> {
    let mut values = Vec::new();
    let mut code = code;
    loop {
        code = parse_skip(code);
        if code.is_empty() || code.starts_with(')') {
            break;
        }
        let (value, next_code) = parse_value(code)?;
        values.push(value);
        code = next_code;
    }
    Ok((values, code))
}

pub fn parse(code: &str) -> Result<Vec<Value>> {
    let (values, excess_code) = parse_list(code)?;
    ensure!(excess_code.is_empty(), "Extra code");
    Ok(values)
}
