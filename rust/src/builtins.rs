use std::rc::Rc;

use anyhow::anyhow;
use anyhow::bail;
use anyhow::Result;

use crate::data::Value;
use crate::environment::Env;

fn builtin_print(args: &[Value]) -> Result<Value> {
    if args.len() != 1 {
        bail!("print: Invalid number of arguments");
    }
    println!("{}", args[0]);
    Ok(Value::Undef)
}

fn builtin_and(args: &[Value]) -> Result<Value> {
    let mut result = true;
    for value in args {
        result &= value.bool();
    }
    Ok(Value::Boolean(result))
}

fn builtin_or(args: &[Value]) -> Result<Value> {
    let mut result = false;
    for value in args {
        result |= value.bool();
    }
    Ok(Value::Boolean(result))
}

fn builtin_not(args: &[Value]) -> Result<Value> {
    if args.len() != 1 {
        bail!("not: Invalid number of arguments");
    }
    let value = &args[0];
    Ok(Value::Boolean(!value.bool()))
}

fn builtin_cons(args: &[Value]) -> Result<Value> {
    if args.len() != 2 {
        bail!("cons: Invalid number of arguments");
    }
    let (car, cdr) = (&args[0], &args[1]);
    Ok(Value::Pair(car.clone().into(), cdr.clone().into()))
}

fn builtin_car(args: &[Value]) -> Result<Value> {
    if args.len() != 1 {
        bail!("car: Invalid number of arguments");
    }
    let (car, _) = args[0].as_pair()?;
    let value = car.get();
    Ok(value)
}

fn builtin_cdr(args: &[Value]) -> Result<Value> {
    if args.len() != 1 {
        bail!("cdr: Invalid number of arguments");
    }
    let (_, cdr) = args[0].as_pair()?;
    let value = cdr.get();
    Ok(value)
}

fn builtin_eq(args: &[Value]) -> Result<Value> {
    if args.len() != 2 {
        bail!("=: Invalid number of arguments");
    }
    let (a, b) = (args[0].as_integer()?, args[1].as_integer()?);
    Ok(Value::Boolean(a == b))
}

fn builtin_lt(args: &[Value]) -> Result<Value> {
    if args.len() != 2 {
        bail!("<: Invalid number of arguments");
    }
    let (a, b) = (args[0].as_integer()?, args[1].as_integer()?);
    Ok(Value::Boolean(a < b))
}

fn builtin_lte(args: &[Value]) -> Result<Value> {
    if args.len() != 2 {
        bail!("<=: Invalid number of arguments");
    }
    let (a, b) = (args[0].as_integer()?, args[1].as_integer()?);
    Ok(Value::Boolean(a <= b))
}

fn builtin_gt(args: &[Value]) -> Result<Value> {
    if args.len() != 2 {
        bail!(">: Invalid number of arguments");
    }
    let (a, b) = (args[0].as_integer()?, args[1].as_integer()?);
    Ok(Value::Boolean(a > b))
}

fn builtin_gte(args: &[Value]) -> Result<Value> {
    if args.len() != 2 {
        bail!(">=: Invalid number of arguments");
    }
    let (a, b) = (args[0].as_integer()?, args[1].as_integer()?);
    Ok(Value::Boolean(a >= b))
}

fn builtin_add(args: &[Value]) -> Result<Value> {
    let mut result = 0;
    for value in args {
        result += value.as_integer()?;
    }
    Ok(Value::Integer(result))
}

fn builtin_sub(args: &[Value]) -> Result<Value> {
    let mut result = args
        .first()
        .ok_or(anyhow!("-: Invalid number of arguments"))?
        .as_integer()?;
    for value in &args[1..] {
        result -= value.as_integer()?;
    }
    Ok(Value::Integer(result))
}

fn builtin_mul(args: &[Value]) -> Result<Value> {
    let mut result = 1;
    for value in args {
        result *= value.as_integer()?;
    }
    Ok(Value::Integer(result))
}

fn builtin_div(args: &[Value]) -> Result<Value> {
    let mut result = args
        .first()
        .ok_or(anyhow!("/: Invalid number of arguments"))?
        .as_integer()?;
    for value in &args[1..] {
        result /= value.as_integer()?;
    }
    Ok(Value::Integer(result))
}

fn builtin_eq_check(args: &[Value]) -> Result<Value> {
    if args.len() != 2 {
        bail!(">: Invalid number of arguments");
    }
    let (lhs, rhs) = (&args[0], &args[1]);
    Ok(Value::Boolean(lhs == rhs))
}

pub fn install(env: &Rc<Env>) {
    for (name, func) in [
        ("print", builtin_print as fn(&[Value]) -> _),
        ("and", builtin_and),
        ("or", builtin_or),
        ("not", builtin_not),
        ("=", builtin_eq),
        ("<", builtin_lt),
        ("<=", builtin_lte),
        (">", builtin_gt),
        (">=", builtin_gte),
        ("+", builtin_add),
        ("-", builtin_sub),
        ("*", builtin_mul),
        ("/", builtin_div),
        ("eq?", builtin_eq_check),
        ("cons", builtin_cons),
        ("car", builtin_car),
        ("cdr", builtin_cdr),
    ] {
        env.ensure(name)
            .set(Value::Function(Rc::new(name.to_owned()), Rc::new(func)));
    }
}
