use std::rc::Rc;

use data::Function;
use data::Value;
use environment::Env;

fn builtin_print(args: Vec<Rc<Value>>) -> Result<Rc<Value>, String> {
    if args.len() != 1 {
        return Err("print: Invalid number of arguments".to_string());
    }
    println!("{}", args[0]);
    Ok(Rc::new(Value::Undef))
}

fn builtin_and(mut args: Vec<Rc<Value>>) -> Result<Rc<Value>, String> {
    let mut result = true;
    for value in args.into_iter() {
        result = result & if let &Value::Boolean(false) = &*value { false } else { true };
    }
    Ok(Rc::new(Value::Boolean(result)))
}

fn builtin_or(mut args: Vec<Rc<Value>>) -> Result<Rc<Value>, String> {
    let mut result = false;
    for value in args.into_iter() {
        result = result | if let &Value::Boolean(false) = &*value { false } else { true };
    }
    Ok(Rc::new(Value::Boolean(result)))
}

fn builtin_not(mut args: Vec<Rc<Value>>) -> Result<Rc<Value>, String> {
    if args.len() != 1 {
        return Err("not: Invalid number of arguments".to_string());
    }
    let value = args.pop().unwrap();
    let bool_value = if let &Value::Boolean(false) = &*value { false } else { true };
    Ok(Rc::new(Value::Boolean(!bool_value)))
}

// FIXME: Can we get rid of this struct and use Fn directly?
struct BuiltinFunction {
    func: &'static Fn(Vec<Rc<Value>>) -> Result<Rc<Value>, String>,
}

impl Function for BuiltinFunction {
    fn apply(&self, args: Vec<Rc<Value>>) -> Result<Rc<Value>, String> {
        // FIXME: Why do we need parentheses here?
        (self.func)(args)
    }
}

fn register(env: &mut Env, name: &str, func: &'static Fn(Vec<Rc<Value>>) -> Result<Rc<Value>, String>) {
    let name_string = name.to_string();
    let var = env.ensure(&name_string);
    var.borrow_mut().value = Rc::new(Value::Function(name_string, Box::new(BuiltinFunction{func: func})));
}

pub fn install(env: &mut Env) {
    register(env, "print", &builtin_print);
    register(env, "and", &builtin_and);
    register(env, "or", &builtin_or);
    register(env, "not", &builtin_not);
}
