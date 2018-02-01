use std::rc::Rc;

use data::Value;
use environment::Env;

fn form_begin(env: &Env, exprs: Vec<Rc<Value>>) -> Result<Rc<Value>, String> {
    let mut result = Ok(Rc::new(Value::Undef));
    for expr in exprs.into_iter() {
        result = env.evaluate(expr);
        if let Err(_) = result {
            return result;
        }
    }
    result
}

// FIXME: Can we get rid of this struct and use Fn directly?
pub struct Form {
    func: &'static Fn(&Env, Vec<Rc<Value>>) -> Result<Rc<Value>, String>,
}

impl Form {
    pub fn apply(&self, env: &Env, exprs: Vec<Rc<Value>>) -> Result<Rc<Value>, String> {
        (self.func)(env, exprs)
    }
}

pub fn lookup(name: &String) -> Option<Form> {
    match name.as_str() {
        "begin" => Some(Form{func: &form_begin}),
        _ => None,
    }
}
