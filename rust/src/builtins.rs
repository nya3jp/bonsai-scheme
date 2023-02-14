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
    let value = car.borrow().clone();
    Ok(value)
}

fn builtin_cdr(args: &[Value]) -> Result<Value> {
    if args.len() != 1 {
        bail!("cdr: Invalid number of arguments");
    }
    let (_, cdr) = args[0].as_pair()?;
    let value = cdr.borrow().clone();
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

fn register(env: &Rc<Env>, name: &str, func: &'static dyn Fn(&[Value]) -> Result<Value>) {
    let name = name.to_string();
    let var = env.ensure(&name);
    *var.borrow_mut() = Value::Function(name, Rc::new(func));
}

pub fn install(env: &Rc<Env>) {
    register(env, "print", &builtin_print);
    register(env, "and", &builtin_and);
    register(env, "or", &builtin_or);
    register(env, "not", &builtin_not);
    register(env, "=", &builtin_eq);
    register(env, "<", &builtin_lt);
    register(env, "<=", &builtin_lte);
    register(env, ">", &builtin_gt);
    register(env, ">=", &builtin_gte);
    register(env, "+", &builtin_add);
    register(env, "-", &builtin_sub);
    register(env, "*", &builtin_mul);
    register(env, "/", &builtin_div);
    register(env, "eq?", &builtin_eq_check);
    register(env, "cons", &builtin_cons);
    register(env, "car", &builtin_car);
    register(env, "cdr", &builtin_cdr);
}
