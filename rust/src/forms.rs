use std::cell::RefCell;
use std::rc::Rc;

use data::Function;
use data::Value;
use environment::Env;

fn evaluate_body(env: &Rc<RefCell<Env>>, body: Vec<Rc<Value>>) -> Result<Rc<Value>, String> {
    let mut value = Rc::new(Value::Undef);
    for expr in body.into_iter() {
        value = Env::evaluate(env, expr)?;
    }
    Ok(value)
}

fn form_quote(env: &Rc<RefCell<Env>>, mut exprs: Vec<Rc<Value>>) -> Result<Rc<Value>, String> {
    if exprs.len() != 1 {
        return Err("args".to_string());
    }
    Ok(exprs.swap_remove(0))
}

fn form_begin(env: &Rc<RefCell<Env>>, exprs: Vec<Rc<Value>>) -> Result<Rc<Value>, String> {
    let mut result = Ok(Rc::new(Value::Undef));
    for expr in exprs.into_iter() {
        result = Env::evaluate(env, expr);
        if let Err(_) = result {
            return result;
        }
    }
    result
}

fn form_if(env: &Rc<RefCell<Env>>, exprs: Vec<Rc<Value>>) -> Result<Rc<Value>, String> {
    if !(exprs.len() == 2 || exprs.len() == 3) {
        return Err("args".to_string());
    }
    let test = Env::evaluate(env, exprs[0].clone())?.bool();
    if test {
        Env::evaluate(env, exprs[1].clone())
    } else if exprs.len() == 3 {
        Env::evaluate(env, exprs[2].clone())
    } else {
        Ok(Rc::new(Value::Undef))
    }
}

fn form_cond(env: &Rc<RefCell<Env>>, exprs: Vec<Rc<Value>>) -> Result<Rc<Value>, String> {
    for expr in exprs.into_iter() {
        let clause = expr.to_native_list()?;
        let test_expr = clause.first().ok_or("cond: Malformed condition".to_string())?;
        let test = if let &Value::Symbol(ref name) = &**test_expr {
            if name.as_str() == "else" {
                true
            } else {
                Env::evaluate(env, test_expr.clone())?.bool()
            }
        } else {
            Env::evaluate(env, test_expr.clone())?.bool()
        };
        if test {
            return evaluate_body(env, clause[1..].to_vec());
        }
    }
    Ok(Rc::new(Value::Undef))
}

struct LambdaFunction {
    env: Rc<RefCell<Env>>,
    params: Vec<String>,
    body: Vec<Rc<Value>>,
}

impl Function for LambdaFunction {
    fn apply(&self, args: Vec<Rc<Value>>) -> Result<Rc<Value>, String> {
        if args.len() != self.params.len() {
            return Err("Wrong number of arguments".to_string());
        }
        let env = Env::new(Some(self.env.clone()));
        for (param, arg) in self.params.iter().zip(args.into_iter()) {
            let var = Env::ensure(&env, param);
            var.borrow_mut().value = arg;
        }
        // FIXME: Avoid copying Vec.
        evaluate_body(&env, self.body.clone())
    }
}

fn make_func_value(env: &Rc<RefCell<Env>>, name: &String, params_value: Rc<Value>, body: Vec<Rc<Value>>) -> Result<Rc<Value>, String> {
    let mut params = vec![];
    for param_value in params_value.to_native_list()?.into_iter() {
        params.push(param_value.as_symbol()?);
    }
    Ok(Rc::new(Value::Function(
        name.clone(), Box::new(LambdaFunction{env: env.clone(), params: params, body: body}))))
}

fn form_define(env: &Rc<RefCell<Env>>, exprs: Vec<Rc<Value>>) -> Result<Rc<Value>, String> {
    let target = exprs.get(0).ok_or("define: Malformed args".to_string())?;
    if let &Value::Symbol(ref name) = &**target {
        if exprs.len() != 2 {
            return Err("define: Excessive args".to_string());
        }
        let value = Env::evaluate(env, exprs[1].clone())?;
        let var = Env::ensure(env, name);
        var.borrow_mut().value = value;
        return Ok(Rc::new(Value::Undef));
    }

    let (car, cdr) = target.as_pair()?;
    let name = car.as_symbol()?;
    let func_value = make_func_value(env, &name, cdr, exprs[1..].to_vec())?;
    let var = Env::ensure(env, &name);
    var.borrow_mut().value = func_value;
    Ok(Rc::new(Value::Undef))
}

fn form_lambda(env: &Rc<RefCell<Env>>, exprs: Vec<Rc<Value>>) -> Result<Rc<Value>, String> {
    let params_value = exprs.get(0).ok_or("lambda: Malformed args".to_string())?;
    make_func_value(env, &"<lambda>".to_string(), params_value.clone(), exprs[1..].to_vec())
}

// FIXME: Can we get rid of this struct and use Fn directly?
pub struct Form {
    func: &'static Fn(&Rc<RefCell<Env>>, Vec<Rc<Value>>) -> Result<Rc<Value>, String>,
}

impl Form {
    pub fn apply(&self, env: &Rc<RefCell<Env>>, exprs: Vec<Rc<Value>>) -> Result<Rc<Value>, String> {
        (self.func)(env, exprs)
    }
}

pub fn lookup(name: &String) -> Option<Form> {
    match name.as_str() {
        "quote" => Some(Form{func: &form_quote}),
        "begin" => Some(Form{func: &form_begin}),
        "if" => Some(Form{func: &form_if}),
        "cond" => Some(Form{func: &form_cond}),
        "define" => Some(Form{func: &form_define}),
        "lambda" => Some(Form{func: &form_lambda}),
        _ => None,
    }
}
