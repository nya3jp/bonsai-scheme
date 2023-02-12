use std::cell::RefCell;
use std::rc::Rc;

use crate::data::Function;
use crate::data::Value;
use crate::data::ValueRef;
use crate::environment::Env;
use crate::error::Error;

fn builtin_print(args: &[ValueRef]) -> Result<ValueRef, Error> {
    if args.len() != 1 {
        return Err(Error::new("print: Invalid number of arguments".into()));
    }
    println!("{}", *args[0].borrow());
    Ok(ValueRef::new(Value::Undef))
}

fn builtin_and(args: &[ValueRef]) -> Result<ValueRef, Error> {
    let mut result = true;
    for value in args.iter() {
        result &= value.bool();
    }
    Ok(ValueRef::new(Value::Boolean(result)))
}

fn builtin_or(args: &[ValueRef]) -> Result<ValueRef, Error> {
    let mut result = false;
    for value in args.iter() {
        result |= value.bool();
    }
    Ok(ValueRef::new(Value::Boolean(result)))
}

fn builtin_not(args: &[ValueRef]) -> Result<ValueRef, Error> {
    if args.len() != 1 {
        return Err(Error::new("not: Invalid number of arguments".into()));
    }
    let value = &args[0];
    Ok(ValueRef::new(Value::Boolean(!value.bool())))
}

fn builtin_cons(args: &[ValueRef]) -> Result<ValueRef, Error> {
    if args.len() != 2 {
        return Err(Error::new("cons: Invalid number of arguments".into()));
    }
    let (car, cdr) = (&args[0], &args[1]);
    Ok(ValueRef::new(Value::Pair(car.clone(), cdr.clone())))
}

fn builtin_car(args: &[ValueRef]) -> Result<ValueRef, Error> {
    if args.len() != 1 {
        return Err(Error::new("car: Invalid number of arguments".into()));
    }
    let (car, _) = args[0].as_pair()?;
    Ok(car)
}

fn builtin_cdr(args: &[ValueRef]) -> Result<ValueRef, Error> {
    if args.len() != 1 {
        return Err(Error::new("cdr: Invalid number of arguments".into()));
    }
    let (_, cdr) = args[0].as_pair()?;
    Ok(cdr)
}

fn builtin_eq(args: &[ValueRef]) -> Result<ValueRef, Error> {
    if args.len() != 2 {
        return Err(Error::new("=: Invalid number of arguments".into()));
    }
    let (a, b) = (args[0].as_integer()?, args[1].as_integer()?);
    Ok(ValueRef::new(Value::Boolean(a == b)))
}

fn builtin_lt(args: &[ValueRef]) -> Result<ValueRef, Error> {
    if args.len() != 2 {
        return Err(Error::new("<: Invalid number of arguments".into()));
    }
    let (a, b) = (args[0].as_integer()?, args[1].as_integer()?);
    Ok(ValueRef::new(Value::Boolean(a < b)))
}

fn builtin_lte(args: &[ValueRef]) -> Result<ValueRef, Error> {
    if args.len() != 2 {
        return Err(Error::new("<=: Invalid number of arguments".into()));
    }
    let (a, b) = (args[0].as_integer()?, args[1].as_integer()?);
    Ok(ValueRef::new(Value::Boolean(a <= b)))
}

fn builtin_gt(args: &[ValueRef]) -> Result<ValueRef, Error> {
    if args.len() != 2 {
        return Err(Error::new(">: Invalid number of arguments".into()));
    }
    let (a, b) = (args[0].as_integer()?, args[1].as_integer()?);
    Ok(ValueRef::new(Value::Boolean(a > b)))
}

fn builtin_gte(args: &[ValueRef]) -> Result<ValueRef, Error> {
    if args.len() != 2 {
        return Err(Error::new(">=: Invalid number of arguments".into()));
    }
    let (a, b) = (args[0].as_integer()?, args[1].as_integer()?);
    Ok(ValueRef::new(Value::Boolean(a >= b)))
}

fn builtin_add(args: &[ValueRef]) -> Result<ValueRef, Error> {
    let mut result = 0;
    for value in args.iter() {
        result += value.as_integer()?;
    }
    Ok(ValueRef::new(Value::Integer(result)))
}

fn builtin_sub(args: &[ValueRef]) -> Result<ValueRef, Error> {
    let mut result = args
        .first()
        .ok_or(Error::new("-: Invalid number of arguments".into()))?
        .as_integer()?;
    for value in &args[1..] {
        result -= value.as_integer()?;
    }
    Ok(ValueRef::new(Value::Integer(result)))
}

fn builtin_mul(args: &[ValueRef]) -> Result<ValueRef, Error> {
    let mut result = 1;
    for value in args.iter() {
        result *= value.as_integer()?;
    }
    Ok(ValueRef::new(Value::Integer(result)))
}

fn builtin_div(args: &[ValueRef]) -> Result<ValueRef, Error> {
    let mut result = args
        .first()
        .ok_or(Error::new("/: Invalid number of arguments".into()))?
        .as_integer()?;
    for value in &args[1..] {
        result /= value.as_integer()?;
    }
    Ok(ValueRef::new(Value::Integer(result)))
}

fn builtin_eq_check(args: &[ValueRef]) -> Result<ValueRef, Error> {
    if args.len() != 2 {
        return Err(Error::new(">: Invalid number of arguments".into()));
    }
    let (lhs, rhs) = (&args[0], &args[1]);
    Ok(ValueRef::new(Value::Boolean(lhs == rhs)))
}

// FIXME: Can we get rid of this struct and use Fn directly?
struct BuiltinFunction {
    func: &'static dyn Fn(&[ValueRef]) -> Result<ValueRef, Error>,
}

impl Function for BuiltinFunction {
    fn apply(&self, args: &[ValueRef]) -> Result<ValueRef, Error> {
        (self.func)(args)
    }
}

fn register(
    env: &Rc<RefCell<Env>>,
    name: &str,
    func: &'static dyn Fn(&[ValueRef]) -> Result<ValueRef, Error>,
) {
    let name_string = name.to_string();
    let var = Env::ensure(env, &name_string);
    var.borrow_mut().value = ValueRef::new(Value::Function(
        name_string,
        Rc::new(BuiltinFunction { func }),
    ));
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
