use std::rc::Rc;

use data::Function;
use data::Value;
use environment::Env;

fn builtin_print(args: Vec<Rc<Value>>) -> Result<Rc<Value>, String> {
    if args.len() != 1 {
        Err("print: Invalid number of arguments".to_string())
    } else {
        println!("{}", args[0]);
        Ok(Rc::new(Value::Undef))
    }
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
}
