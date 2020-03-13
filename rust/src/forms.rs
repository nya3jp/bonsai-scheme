use std::cell::RefCell;
use std::rc::Rc;

use data::Function;
use data::Value;
use data::ValueRef;
use environment::Env;
use std::borrow::BorrowMut;

fn evaluate_body(env: &Rc<RefCell<Env>>, body: &[ValueRef]) -> Result<ValueRef, String> {
    let mut value = ValueRef::new(Value::Undef);
    for expr in body.iter() {
        value = Env::evaluate(env, expr)?;
    }
    Ok(value)
}

fn form_quote(_: &Rc<RefCell<Env>>, exprs: &[ValueRef]) -> Result<ValueRef, String> {
    if exprs.len() != 1 {
        return Err("args".to_string());
    }
    Ok(exprs[0].clone())
}

fn form_begin(env: &Rc<RefCell<Env>>, exprs: &[ValueRef]) -> Result<ValueRef, String> {
    let mut result = Ok(ValueRef::new(Value::Undef));
    for expr in exprs.iter() {
        result = Env::evaluate(env, expr);
        if let Err(_) = result {
            return result;
        }
    }
    result
}

fn form_if(env: &Rc<RefCell<Env>>, exprs: &[ValueRef]) -> Result<ValueRef, String> {
    if !(exprs.len() == 2 || exprs.len() == 3) {
        return Err("args".to_string());
    }
    let test = Env::evaluate(env, &exprs[0])?.bool();
    if test {
        Env::evaluate(env, &exprs[1])
    } else if exprs.len() == 3 {
        Env::evaluate(env, &exprs[2])
    } else {
        Ok(ValueRef::new(Value::Undef))
    }
}

fn form_cond(env: &Rc<RefCell<Env>>, exprs: &[ValueRef]) -> Result<ValueRef, String> {
    for expr in exprs.iter() {
        let clause = expr.to_native_list()?;
        let test_expr = clause.first().ok_or("cond: Malformed condition".to_string())?;
        let test = match test_expr.as_symbol() {
            Ok(name) if name.as_str() == "else" => true,
            _ => Env::evaluate(env, test_expr)?.bool(),
        };
        if test {
            return evaluate_body(env, &clause[1..]);
        }
    }
    Ok(ValueRef::new(Value::Undef))
}

struct LambdaFunction {
    env: Rc<RefCell<Env>>,
    params: Vec<String>,
    body: Vec<ValueRef>,
}

impl Function for LambdaFunction {
    fn apply(&self, args: &[ValueRef]) -> Result<ValueRef, String> {
        if args.len() != self.params.len() {
            return Err("Wrong number of arguments".to_string());
        }
        let env = Env::new(Some(self.env.clone()));
        for (param, arg) in self.params.iter().zip(args.iter()) {
            let mut var = Env::ensure(&env, param);
            (*var).borrow_mut().value = arg.clone();
        }
        evaluate_body(&env, self.body.as_slice())
    }
}

fn make_func_value(env: &Rc<RefCell<Env>>, name: &str, params_value: &ValueRef, body: &[ValueRef]) -> Result<ValueRef, String> {
    let mut params = vec![];
    for param_value in params_value.to_native_list()?.into_iter() {
        params.push(param_value.as_symbol()?);
    }
    Ok(ValueRef::new(Value::Function(
        name.to_string(),
        Rc::new(LambdaFunction{env: env.clone(), params: params, body: body.to_vec()}))))
}

fn form_define(env: &Rc<RefCell<Env>>, exprs: &[ValueRef]) -> Result<ValueRef, String> {
    let target = exprs.get(0).ok_or("define: Malformed args".to_string())?;
    if let Ok(name) = target.as_symbol() {
        if exprs.len() != 2 {
            return Err("define: Excessive args".to_string());
        }
        let value = Env::evaluate(env, &exprs[1])?;
        let var = Env::ensure(env, &name);
        (*var).borrow_mut().value = value.clone();
        return Ok(ValueRef::new(Value::Undef));
    }

    let (car, cdr) = target.as_pair()?;
    let name = car.as_symbol()?;
    let func_value = make_func_value(env, &name, &cdr, &exprs[1..])?;
    let var = Env::ensure(env, &name);
    (*var).borrow_mut().value = func_value.clone();
    Ok(ValueRef::new(Value::Undef))
}

fn form_lambda(env: &Rc<RefCell<Env>>, exprs: &[ValueRef]) -> Result<ValueRef, String> {
    let params_value = exprs.get(0).ok_or("lambda: Malformed args".to_string())?;
    make_func_value(env, "<lambda>", params_value, &exprs[1..])
}

fn form_let(env: &Rc<RefCell<Env>>, exprs: &[ValueRef]) -> Result<ValueRef, String> {
    let let_env = Env::new(Some(env.clone()));
    let bindings_value =
        exprs.get(0).ok_or("let: Malformed args".to_string())?;
    for binding_value in bindings_value.to_native_list()?.iter() {
        let binding = binding_value.to_native_list()?;
        if binding.len() != 2 {
            return Err("let: Malformed binding".to_string());
        }
        let name = binding[0].as_symbol()?;
        let value = Env::evaluate(env, &binding[1])?;
        let var = Env::ensure(&let_env, &name);
        (*var).borrow_mut().value = value.clone();
    }
    evaluate_body(&let_env, &exprs[1..])
}

fn form_let_star(env: &Rc<RefCell<Env>>, exprs: &[ValueRef]) -> Result<ValueRef, String> {
    let mut let_env = env.clone();
    let bindings_value =
        exprs.get(0).ok_or("let: Malformed args".to_string())?;
    for binding_value in bindings_value.to_native_list()?.iter() {
        let binding = binding_value.to_native_list()?;
        if binding.len() != 2 {
            return Err("let: Malformed binding".to_string());
        }
        let name = binding[0].as_symbol()?;
        let parent_env = let_env.clone();
        let_env = Env::new(Some(let_env.clone()));
        let value = Env::evaluate(&parent_env, &binding[1])?;
        let var = Env::ensure(&let_env, &name);
        (*var).borrow_mut().value = value.clone();
    }
    evaluate_body(&let_env, &exprs[1..])
}

fn form_letrec(env: &Rc<RefCell<Env>>, exprs: &[ValueRef]) -> Result<ValueRef, String> {
    let let_env = Env::new(Some(env.clone()));
    let bindings_value =
        exprs.get(0).ok_or("let: Malformed args".to_string())?;
    for binding_value in bindings_value.to_native_list()?.iter() {
        let binding = binding_value.to_native_list()?;
        if binding.len() != 2 {
            return Err("let: Malformed binding".to_string());
        }
        let name = binding[0].as_symbol()?;
        let value = Env::evaluate(&let_env, &binding[1])?;
        let var = Env::ensure(&let_env, &name);
        (*var).borrow_mut().value = value.clone();
    }
    evaluate_body(&let_env, &exprs[1..])
}

fn form_set(env: &Rc<RefCell<Env>>, exprs: &[ValueRef]) -> Result<ValueRef, String> {
    if exprs.len() != 2 {
        return Err("set!: Invalid number of args".to_string());
    }
    let name = exprs[0].as_symbol()?;
    let value = Env::evaluate(env, &exprs[1])?;
    let var =
        Env::lookup_ref(env, &name).ok_or("set!: Name not found".to_string())?;
    (*var).borrow_mut().value = value.clone();
    Ok(ValueRef::new(Value::Undef))
}

fn form_set_car(env: &Rc<RefCell<Env>>, exprs: &[ValueRef]) -> Result<ValueRef, String> {
    if exprs.len() != 2 {
        return Err("set!: Invalid number of args".to_string());
    }
    let target = Env::evaluate(env, &exprs[0])?;
    let value = Env::evaluate(env, &exprs[1])?;
    let mut r = target.borrow_mut();
    if let &mut Value::Pair(ref mut car, _) = &mut *r {
        *car = value;
    } else {
        return Err("set-car!: Not a pair".to_string());
    }
    Ok(ValueRef::new(Value::Undef))
}

fn form_set_cdr(env: &Rc<RefCell<Env>>, exprs: &[ValueRef]) -> Result<ValueRef, String> {
    if exprs.len() != 2 {
        return Err("set!: Invalid number of args".to_string());
    }
    let target = Env::evaluate(env, &exprs[0])?;
    let value = Env::evaluate(env, &exprs[1])?;
    let mut r = target.borrow_mut();
    if let &mut Value::Pair(_, ref mut cdr) = &mut *r {
        *cdr = value;
    } else {
        return Err("set-cdr!: Not a pair".to_string());
    }
    Ok(ValueRef::new(Value::Undef))
}

// FIXME: Can we get rid of this struct and use Fn directly?
pub struct Form {
    func: &'static Fn(&Rc<RefCell<Env>>, &[ValueRef]) -> Result<ValueRef, String>,
}

impl Form {
    pub fn apply(&self, env: &Rc<RefCell<Env>>, exprs: &[ValueRef]) -> Result<ValueRef, String> {
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
        "let" => Some(Form{func: &form_let}),
        "let*" => Some(Form{func: &form_let_star}),
        "letrec" => Some(Form{func: &form_letrec}),
        "set!" => Some(Form{func: &form_set}),
        "set-car!" => Some(Form{func: &form_set_car}),
        "set-cdr!" => Some(Form{func: &form_set_cdr}),
        _ => None,
    }
}
