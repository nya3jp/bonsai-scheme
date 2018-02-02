use std::cell::RefCell;
use std::rc::Rc;

use data::Function;
use data::Value;
use environment::Env;

fn builtin_print(args: &[Rc<Value>]) -> Result<Rc<Value>, String> {
    if args.len() != 1 {
        return Err("print: Invalid number of arguments".to_string());
    }
    println!("{}", args[0]);
    Ok(Rc::new(Value::Undef))
}

fn builtin_and(args: &[Rc<Value>]) -> Result<Rc<Value>, String> {
    let mut result = true;
    for value in args.iter() {
        result = result & value.bool();
    }
    Ok(Rc::new(Value::Boolean(result)))
}

fn builtin_or(args: &[Rc<Value>]) -> Result<Rc<Value>, String> {
    let mut result = false;
    for value in args.iter() {
        result = result | value.bool();
    }
    Ok(Rc::new(Value::Boolean(result)))
}

fn builtin_not(args: &[Rc<Value>]) -> Result<Rc<Value>, String> {
    if args.len() != 1 {
        return Err("not: Invalid number of arguments".to_string());
    }
    let value = &args[0];
    Ok(Rc::new(Value::Boolean(!value.bool())))
}

fn builtin_cons(args: &[Rc<Value>]) -> Result<Rc<Value>, String> {
    if args.len() != 2 {
        return Err("cons: Invalid number of arguments".to_string());
    }
    let (car, cdr) = (&args[0], &args[1]);
    Ok(Rc::new(Value::Pair(car.clone(), cdr.clone())))
}

fn builtin_car(args: &[Rc<Value>]) -> Result<Rc<Value>, String> {
    if args.len() != 1 {
        return Err("car: Invalid number of arguments".to_string());
    }
    let value = &args[0];
    if let &Value::Pair(ref car, _) = &**value {
        return Ok(car.clone());
    }
    Err("car: Not a pair".to_string())
}

fn builtin_cdr(args: &[Rc<Value>]) -> Result<Rc<Value>, String> {
    if args.len() != 1 {
        return Err("cdr: Invalid number of arguments".to_string());
    }
    let value = &args[0];
    if let &Value::Pair(_, ref cdr) = &**value {
        return Ok(cdr.clone());
    }
    Err("cdr: Not a pair".to_string())
}

fn builtin_eq(args: &[Rc<Value>]) -> Result<Rc<Value>, String> {
    if args.len() != 2 {
        return Err("=: Invalid number of arguments".to_string());
    }
    let (a, b) = (args[0].as_integer()?, args[1].as_integer()?);
    Ok(Rc::new(Value::Boolean(a == b)))
}

fn builtin_lt(args: &[Rc<Value>]) -> Result<Rc<Value>, String> {
    if args.len() != 2 {
        return Err("<: Invalid number of arguments".to_string());
    }
    let (a, b) = (args[0].as_integer()?, args[1].as_integer()?);
    Ok(Rc::new(Value::Boolean(a < b)))
}

fn builtin_lte(args: &[Rc<Value>]) -> Result<Rc<Value>, String> {
    if args.len() != 2 {
        return Err("<=: Invalid number of arguments".to_string());
    }
    let (a, b) = (args[0].as_integer()?, args[1].as_integer()?);
    Ok(Rc::new(Value::Boolean(a <= b)))
}

fn builtin_gt(args: &[Rc<Value>]) -> Result<Rc<Value>, String> {
    if args.len() != 2 {
        return Err(">: Invalid number of arguments".to_string());
    }
    let (a, b) = (args[0].as_integer()?, args[1].as_integer()?);
    Ok(Rc::new(Value::Boolean(a > b)))
}

fn builtin_gte(args: &[Rc<Value>]) -> Result<Rc<Value>, String> {
    if args.len() != 2 {
        return Err(">=: Invalid number of arguments".to_string());
    }
    let (a, b) = (args[0].as_integer()?, args[1].as_integer()?);
    Ok(Rc::new(Value::Boolean(a >= b)))
}

fn builtin_add(args: &[Rc<Value>]) -> Result<Rc<Value>, String> {
    let mut result = 0;
    for value in args.iter() {
        result += value.as_integer()?;
    }
    Ok(Rc::new(Value::Integer(result)))
}

fn builtin_sub(args: &[Rc<Value>]) -> Result<Rc<Value>, String> {
    let mut result = args.first().ok_or("-: Invalid number of arguments".to_string())?.as_integer()?;
    for value in &args[1..] {
        result -= value.as_integer()?;
    }
    Ok(Rc::new(Value::Integer(result)))
}

fn builtin_mul(args: &[Rc<Value>]) -> Result<Rc<Value>, String> {
    let mut result = 1;
    for value in args.iter() {
        result *= value.as_integer()?;
    }
    Ok(Rc::new(Value::Integer(result)))
}

fn builtin_div(args: &[Rc<Value>]) -> Result<Rc<Value>, String> {
    let mut result = args.first().ok_or("/: Invalid number of arguments".to_string())?.as_integer()?;
    for value in &args[1..] {
        result /= value.as_integer()?;
    }
    Ok(Rc::new(Value::Integer(result)))
}

fn builtin_eq_check(args: &[Rc<Value>]) -> Result<Rc<Value>, String> {
    if args.len() != 2 {
        return Err(">: Invalid number of arguments".to_string());
    }
    let (lhs, rhs) = (&args[0], &args[1]);
    Ok(Rc::new(Value::Boolean(lhs == rhs)))
}

// FIXME: Can we get rid of this struct and use Fn directly?
struct BuiltinFunction {
    func: &'static Fn(&[Rc<Value>]) -> Result<Rc<Value>, String>,
}

impl Function for BuiltinFunction {
    fn apply(&self, args: &[Rc<Value>]) -> Result<Rc<Value>, String> {
        (self.func)(args)
    }
}

fn register(env: &Rc<RefCell<Env>>, name: &str, func: &'static Fn(&[Rc<Value>]) -> Result<Rc<Value>, String>) {
    let name_string = name.to_string();
    let var = Env::ensure(env, &name_string);
    var.borrow_mut().value = Rc::new(Value::Function(name_string, Box::new(BuiltinFunction{func: func})));
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
