use std::cell::RefCell;
use std::rc::Rc;

use data::Function;
use data::Value;
use environment::Env;

fn evaluate_body(env: &Rc<RefCell<Env>>, body: &[Rc<Value>]) -> Result<Rc<Value>, String> {
    let mut value = Rc::new(Value::Undef);
    for expr in body.iter() {
        value = Env::evaluate(env, expr)?;
    }
    Ok(value)
}

fn form_quote(_: &Rc<RefCell<Env>>, exprs: &[Rc<Value>]) -> Result<Rc<Value>, String> {
    if exprs.len() != 1 {
        return Err("args".to_string());
    }
    Ok(exprs[0].clone())
}

fn form_begin(env: &Rc<RefCell<Env>>, exprs: &[Rc<Value>]) -> Result<Rc<Value>, String> {
    let mut result = Ok(Rc::new(Value::Undef));
    for expr in exprs.iter() {
        result = Env::evaluate(env, expr);
        if let Err(_) = result {
            return result;
        }
    }
    result
}

fn form_if(env: &Rc<RefCell<Env>>, exprs: &[Rc<Value>]) -> Result<Rc<Value>, String> {
    if !(exprs.len() == 2 || exprs.len() == 3) {
        return Err("args".to_string());
    }
    let test = Env::evaluate(env, &exprs[0])?.bool();
    if test {
        Env::evaluate(env, &exprs[1])
    } else if exprs.len() == 3 {
        Env::evaluate(env, &exprs[2])
    } else {
        Ok(Rc::new(Value::Undef))
    }
}

fn form_cond(env: &Rc<RefCell<Env>>, exprs: &[Rc<Value>]) -> Result<Rc<Value>, String> {
    for expr in exprs.iter() {
        let clause = expr.to_native_list()?;
        let test_expr = clause.first().ok_or("cond: Malformed condition".to_string())?;
        let test = if let &Value::Symbol(ref name) = &**test_expr {
            if name.as_str() == "else" {
                true
            } else {
                Env::evaluate(env, test_expr)?.bool()
            }
        } else {
            Env::evaluate(env, test_expr)?.bool()
        };
        if test {
            return evaluate_body(env, &clause[1..]);
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
    fn apply(&self, args: &[Rc<Value>]) -> Result<Rc<Value>, String> {
        if args.len() != self.params.len() {
            return Err("Wrong number of arguments".to_string());
        }
        let env = Env::new(Some(self.env.clone()));
        for (param, arg) in self.params.iter().zip(args.iter()) {
            let var = Env::ensure(&env, param);
            var.borrow_mut().value = arg.clone();
        }
        evaluate_body(&env, self.body.as_slice())
    }
}

fn make_func_value(env: &Rc<RefCell<Env>>, name: &str, params_value: &Rc<Value>, body: &[Rc<Value>]) -> Result<Rc<Value>, String> {
    let mut params = vec![];
    for param_value in params_value.to_native_list()?.into_iter() {
        params.push(param_value.as_symbol()?);
    }
    Ok(Rc::new(Value::Function(
        name.to_string(),
        Box::new(LambdaFunction{env: env.clone(), params: params, body: body.to_vec()}))))
}

fn form_define(env: &Rc<RefCell<Env>>, exprs: &[Rc<Value>]) -> Result<Rc<Value>, String> {
    let target = exprs.get(0).ok_or("define: Malformed args".to_string())?;
    if let &Value::Symbol(ref name) = &**target {
        if exprs.len() != 2 {
            return Err("define: Excessive args".to_string());
        }
        let value = Env::evaluate(env, &exprs[1])?;
        let var = Env::ensure(env, &name);
        var.borrow_mut().value = value;
        return Ok(Rc::new(Value::Undef));
    }

    let (car, cdr) = target.as_pair()?;
    let name = car.as_symbol()?;
    let func_value = make_func_value(env, &name, &cdr, &exprs[1..])?;
    let var = Env::ensure(env, &name);
    var.borrow_mut().value = func_value;
    Ok(Rc::new(Value::Undef))
}

fn form_lambda(env: &Rc<RefCell<Env>>, exprs: &[Rc<Value>]) -> Result<Rc<Value>, String> {
    let params_value = exprs.get(0).ok_or("lambda: Malformed args".to_string())?;
    make_func_value(env, "<lambda>", params_value, &exprs[1..])
}

// FIXME: Can we get rid of this struct and use Fn directly?
pub struct Form {
    func: &'static Fn(&Rc<RefCell<Env>>, &[Rc<Value>]) -> Result<Rc<Value>, String>,
}

impl Form {
    pub fn apply(&self, env: &Rc<RefCell<Env>>, exprs: &[Rc<Value>]) -> Result<Rc<Value>, String> {
        (self.func)(env, exprs)
    }
}

pub fn lookup(name: &str) -> Option<Form> {
    match name {
        "quote" => Some(Form{func: &form_quote}),
        "begin" => Some(Form{func: &form_begin}),
        "if" => Some(Form{func: &form_if}),
        "cond" => Some(Form{func: &form_cond}),
        "define" => Some(Form{func: &form_define}),
        "lambda" => Some(Form{func: &form_lambda}),
        _ => None,
    }
}
