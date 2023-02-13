use std::cell::RefCell;
use std::rc::Rc;

use anyhow::anyhow;
use anyhow::bail;
use anyhow::Result;

use crate::data::Value;
use crate::data::ValueRef;
use crate::environment::Env;

fn builtin_print(args: &[ValueRef]) -> Result<ValueRef> {
    if args.len() != 1 {
        bail!("print: Invalid number of arguments");
    }
    println!("{}", *args[0].borrow());
    Ok(ValueRef::new(Value::Undef))
}

fn builtin_and(args: &[ValueRef]) -> Result<ValueRef> {
    let mut result = true;
    for value in args.iter() {
        result &= value.bool();
    }
    Ok(ValueRef::new(Value::Boolean(result)))
}

fn builtin_or(args: &[ValueRef]) -> Result<ValueRef> {
    let mut result = false;
    for value in args.iter() {
        result |= value.bool();
    }
    Ok(ValueRef::new(Value::Boolean(result)))
}

fn builtin_not(args: &[ValueRef]) -> Result<ValueRef> {
    if args.len() != 1 {
        bail!("not: Invalid number of arguments");
    }
    let value = &args[0];
    Ok(ValueRef::new(Value::Boolean(!value.bool())))
}

fn builtin_cons(args: &[ValueRef]) -> Result<ValueRef> {
    if args.len() != 2 {
        bail!("cons: Invalid number of arguments");
    }
    let (car, cdr) = (&args[0], &args[1]);
    Ok(ValueRef::new(Value::Pair(car.clone(), cdr.clone())))
}

fn builtin_car(args: &[ValueRef]) -> Result<ValueRef> {
    if args.len() != 1 {
        bail!("car: Invalid number of arguments");
    }
    let (car, _) = args[0].as_pair()?;
    Ok(car)
}

fn builtin_cdr(args: &[ValueRef]) -> Result<ValueRef> {
    if args.len() != 1 {
        bail!("cdr: Invalid number of arguments");
    }
    let (_, cdr) = args[0].as_pair()?;
    Ok(cdr)
}

fn builtin_eq(args: &[ValueRef]) -> Result<ValueRef> {
    if args.len() != 2 {
        bail!("=: Invalid number of arguments");
    }
    let (a, b) = (args[0].as_integer()?, args[1].as_integer()?);
    Ok(ValueRef::new(Value::Boolean(a == b)))
}

fn builtin_lt(args: &[ValueRef]) -> Result<ValueRef> {
    if args.len() != 2 {
        bail!("<: Invalid number of arguments");
    }
    let (a, b) = (args[0].as_integer()?, args[1].as_integer()?);
    Ok(ValueRef::new(Value::Boolean(a < b)))
}

fn builtin_lte(args: &[ValueRef]) -> Result<ValueRef> {
    if args.len() != 2 {
        bail!("<=: Invalid number of arguments");
    }
    let (a, b) = (args[0].as_integer()?, args[1].as_integer()?);
    Ok(ValueRef::new(Value::Boolean(a <= b)))
}

fn builtin_gt(args: &[ValueRef]) -> Result<ValueRef> {
    if args.len() != 2 {
        bail!(">: Invalid number of arguments");
    }
    let (a, b) = (args[0].as_integer()?, args[1].as_integer()?);
    Ok(ValueRef::new(Value::Boolean(a > b)))
}

fn builtin_gte(args: &[ValueRef]) -> Result<ValueRef> {
    if args.len() != 2 {
        bail!(">=: Invalid number of arguments");
    }
    let (a, b) = (args[0].as_integer()?, args[1].as_integer()?);
    Ok(ValueRef::new(Value::Boolean(a >= b)))
}

fn builtin_add(args: &[ValueRef]) -> Result<ValueRef> {
    let mut result = 0;
    for value in args.iter() {
        result += value.as_integer()?;
    }
    Ok(ValueRef::new(Value::Integer(result)))
}

fn builtin_sub(args: &[ValueRef]) -> Result<ValueRef> {
    let mut result = args
        .first()
        .ok_or(anyhow!("-: Invalid number of arguments"))?
        .as_integer()?;
    for value in &args[1..] {
        result -= value.as_integer()?;
    }
    Ok(ValueRef::new(Value::Integer(result)))
}

fn builtin_mul(args: &[ValueRef]) -> Result<ValueRef> {
    let mut result = 1;
    for value in args.iter() {
        result *= value.as_integer()?;
    }
    Ok(ValueRef::new(Value::Integer(result)))
}

fn builtin_div(args: &[ValueRef]) -> Result<ValueRef> {
    let mut result = args
        .first()
        .ok_or(anyhow!("/: Invalid number of arguments"))?
        .as_integer()?;
    for value in &args[1..] {
        result /= value.as_integer()?;
    }
    Ok(ValueRef::new(Value::Integer(result)))
}

fn builtin_eq_check(args: &[ValueRef]) -> Result<ValueRef> {
    if args.len() != 2 {
        bail!(">: Invalid number of arguments");
    }
    let (lhs, rhs) = (&args[0], &args[1]);
    Ok(ValueRef::new(Value::Boolean(lhs == rhs)))
}

fn register(
    env: &Rc<RefCell<Env>>,
    name: &str,
    func: &'static dyn Fn(&[ValueRef]) -> Result<ValueRef>,
) {
    let name_string = name.to_string();
    let var = Env::ensure(env, &name_string);
    var.borrow_mut().value = ValueRef::new(Value::Function(name_string, Rc::new(func)));
}

pub fn install(env: &Rc<RefCell<Env>>) {
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
