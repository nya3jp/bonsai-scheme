use std::cell::RefCell;
use std::rc::Rc;

use anyhow::anyhow;
use anyhow::bail;
use anyhow::Result;

use crate::data::Function;
use crate::data::Value;
use crate::environment::Env;

fn evaluate_body(env: &Rc<RefCell<Env>>, body: &[Value]) -> Result<Value> {
    let mut value = Value::Undef;
    for expr in body.iter() {
        value = Env::evaluate(env, expr)?;
    }
    Ok(value)
}

fn form_quote(_: &Rc<RefCell<Env>>, exprs: &[Value]) -> Result<Value> {
    if exprs.len() != 1 {
        bail!("args");
    }
    Ok(exprs[0].clone())
}

fn form_begin(env: &Rc<RefCell<Env>>, exprs: &[Value]) -> Result<Value> {
    let mut result = Value::Undef;
    for expr in exprs.iter() {
        result = Env::evaluate(env, expr)?;
    }
    Ok(result)
}

fn form_if(env: &Rc<RefCell<Env>>, exprs: &[Value]) -> Result<Value> {
    if !(exprs.len() == 2 || exprs.len() == 3) {
        bail!("args");
    }
    let test = Env::evaluate(env, &exprs[0])?.bool();
    if test {
        Env::evaluate(env, &exprs[1])
    } else if exprs.len() == 3 {
        Env::evaluate(env, &exprs[2])
    } else {
        Ok(Value::Undef)
    }
}

fn form_cond(env: &Rc<RefCell<Env>>, exprs: &[Value]) -> Result<Value> {
    for expr in exprs.iter() {
        let clause = expr.to_native_list()?;
        let test_expr = clause.first().ok_or(anyhow!("cond: Malformed condition"))?;
        let test = match test_expr.as_symbol() {
            Ok(name) if name.as_str() == "else" => true,
            _ => Env::evaluate(env, test_expr)?.bool(),
        };
        if test {
            return evaluate_body(env, &clause[1..]);
        }
    }
    Ok(Value::Undef)
}

struct LambdaFunction {
    env: Rc<RefCell<Env>>,
    params: Vec<String>,
    body: Vec<Value>,
}

impl Function for LambdaFunction {
    fn apply(&self, args: &[Value]) -> Result<Value> {
        if args.len() != self.params.len() {
            bail!("Wrong number of arguments");
        }
        let env = Env::new(Some(self.env.clone()));
        for (param, arg) in self.params.iter().zip(args.iter()) {
            let var = Env::ensure(&env, param);
            *var.borrow_mut() = arg.clone();
        }
        evaluate_body(&env, self.body.as_slice())
    }
}

fn make_func_value(
    env: &Rc<RefCell<Env>>,
    name: &str,
    params_value: &Value,
    body: &[Value],
) -> Result<Value> {
    let mut params = vec![];
    for param_value in params_value.to_native_list()?.into_iter() {
        params.push(param_value.as_symbol()?);
    }
    Ok(Value::Function(
        name.to_string(),
        Rc::new(LambdaFunction {
            env: env.clone(),
            params,
            body: body.to_vec(),
        }),
    ))
}

fn form_define(env: &Rc<RefCell<Env>>, exprs: &[Value]) -> Result<Value> {
    let target = exprs.get(0).ok_or(anyhow!("define: Malformed args"))?;
    if let Ok(name) = target.as_symbol() {
        if exprs.len() != 2 {
            bail!("define: Excessive args");
        }
        let value = Env::evaluate(env, &exprs[1])?;
        let var = Env::ensure(env, &name);
        *var.borrow_mut() = value;
        return Ok(Value::Undef);
    }

    let (car, cdr) = target.as_pair()?;
    let name = car.borrow().as_symbol()?;
    let func_value = make_func_value(env, &name, &*cdr.borrow(), &exprs[1..])?;
    let var = Env::ensure(env, &name);
    *var.borrow_mut() = func_value;
    Ok(Value::Undef)
}

fn form_lambda(env: &Rc<RefCell<Env>>, exprs: &[Value]) -> Result<Value> {
    let params_value = exprs.get(0).ok_or(anyhow!("lambda: Malformed args"))?;
    make_func_value(env, "<lambda>", params_value, &exprs[1..])
}

fn form_let(env: &Rc<RefCell<Env>>, exprs: &[Value]) -> Result<Value> {
    let let_env = Env::new(Some(env.clone()));
    let bindings_value = exprs.get(0).ok_or(anyhow!("let: Malformed args"))?;
    for binding_value in bindings_value.to_native_list()?.iter() {
        let binding = binding_value.to_native_list()?;
        if binding.len() != 2 {
            bail!("let: Malformed binding");
        }
        let name = binding[0].as_symbol()?;
        let value = Env::evaluate(env, &binding[1])?;
        let var = Env::ensure(&let_env, &name);
        *var.borrow_mut() = value.clone();
    }
    evaluate_body(&let_env, &exprs[1..])
}

fn form_let_star(env: &Rc<RefCell<Env>>, exprs: &[Value]) -> Result<Value> {
    let mut let_env = env.clone();
    let bindings_value = exprs.get(0).ok_or(anyhow!("let: Malformed args"))?;
    for binding_value in bindings_value.to_native_list()?.iter() {
        let binding = binding_value.to_native_list()?;
        if binding.len() != 2 {
            bail!("let: Malformed binding");
        }
        let name = binding[0].as_symbol()?;
        let parent_env = let_env.clone();
        let_env = Env::new(Some(let_env.clone()));
        let value = Env::evaluate(&parent_env, &binding[1])?;
        let var = Env::ensure(&let_env, &name);
        *var.borrow_mut() = value.clone();
    }
    evaluate_body(&let_env, &exprs[1..])
}

fn form_letrec(env: &Rc<RefCell<Env>>, exprs: &[Value]) -> Result<Value> {
    let let_env = Env::new(Some(env.clone()));
    let bindings_value = exprs.get(0).ok_or(anyhow!("let: Malformed args"))?;
    for binding_value in bindings_value.to_native_list()?.iter() {
        let binding = binding_value.to_native_list()?;
        if binding.len() != 2 {
            bail!("let: Malformed binding");
        }
        let name = binding[0].as_symbol()?;
        let value = Env::evaluate(&let_env, &binding[1])?;
        let var = Env::ensure(&let_env, &name);
        *var.borrow_mut() = value.clone();
    }
    evaluate_body(&let_env, &exprs[1..])
}

fn form_set(env: &Rc<RefCell<Env>>, exprs: &[Value]) -> Result<Value> {
    if exprs.len() != 2 {
        bail!("set!: Invalid number of args");
    }
    let name = exprs[0].as_symbol()?;
    let value = Env::evaluate(env, &exprs[1])?;
    let var = Env::lookup_ref(env, &name).ok_or(anyhow!("set!: Name not found"))?;
    *var.borrow_mut() = value;
    Ok(Value::Undef)
}

fn form_set_car(env: &Rc<RefCell<Env>>, exprs: &[Value]) -> Result<Value> {
    if exprs.len() != 2 {
        bail!("set!: Invalid number of args");
    }
    let target = Env::evaluate(env, &exprs[0])?;
    let value = Env::evaluate(env, &exprs[1])?;
    if let Value::Pair(car, _) = target {
        *car.borrow_mut() = value;
    } else {
        bail!("set-car!: Not a pair");
    }
    Ok(Value::Undef)
}

fn form_set_cdr(env: &Rc<RefCell<Env>>, exprs: &[Value]) -> Result<Value> {
    if exprs.len() != 2 {
        bail!("set!: Invalid number of args");
    }
    let target = Env::evaluate(env, &exprs[0])?;
    let value = Env::evaluate(env, &exprs[1])?;
    if let Value::Pair(_, cdr) = target {
        *cdr.borrow_mut() = value;
    } else {
        bail!("set-cdr!: Not a pair");
    }
    Ok(Value::Undef)
}

// FIXME: Can we get rid of this struct and use Fn directly?
pub struct Form {
    func: &'static dyn Fn(&Rc<RefCell<Env>>, &[Value]) -> Result<Value>,
}

impl Form {
    pub fn apply(&self, env: &Rc<RefCell<Env>>, exprs: &[Value]) -> Result<Value> {
        (self.func)(env, exprs)
    }
}

pub fn lookup(name: &str) -> Option<Form> {
    match name {
        "quote" => Some(Form { func: &form_quote }),
        "begin" => Some(Form { func: &form_begin }),
        "if" => Some(Form { func: &form_if }),
        "cond" => Some(Form { func: &form_cond }),
        "define" => Some(Form { func: &form_define }),
        "lambda" => Some(Form { func: &form_lambda }),
        "let" => Some(Form { func: &form_let }),
        "let*" => Some(Form {
            func: &form_let_star,
        }),
        "letrec" => Some(Form { func: &form_letrec }),
        "set!" => Some(Form { func: &form_set }),
        "set-car!" => Some(Form {
            func: &form_set_car,
        }),
        "set-cdr!" => Some(Form {
            func: &form_set_cdr,
        }),
        _ => None,
    }
}
