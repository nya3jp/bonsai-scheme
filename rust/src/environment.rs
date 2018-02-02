use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use builtins;
use data::Value;
use forms;

pub struct Variable {
    pub value: Rc<Value>,
}

impl Variable {
    pub fn new() -> Variable {
        Variable{value: Rc::new(Value::Undef)}
    }
}

pub struct Env {
    parent: Option<Rc<RefCell<Env>>>,
    vars: HashMap<String, Rc<RefCell<Variable>>>,
}

impl Env {
    pub fn new_top_level() -> Rc<RefCell<Env>> {
        let mut env = Env::new(None);
        builtins::install(&env);
        env
    }

    pub fn new(parent: Option<Rc<RefCell<Env>>>) -> Rc<RefCell<Env>> {
        Rc::new(RefCell::new(Env{parent: parent, vars: HashMap::new()}))
    }

    pub fn ensure(env: &Rc<RefCell<Env>>, name: &String) -> Rc<RefCell<Variable>> {
        let mut env_ref = env.borrow_mut();
        if !env_ref.vars.contains_key(name) {
            env_ref.vars.insert(name.clone(), Rc::new(RefCell::new(Variable::new())));
        }
        env_ref.vars.get(name).unwrap().clone()
    }

    pub fn lookup(env: &Rc<RefCell<Env>>, name: &String) -> Option<Rc<RefCell<Variable>>> {
        if env.borrow().vars.contains_key(name) {
            Some(env.borrow().vars.get(name).unwrap().clone())
        } else if let Some(ref parent) = env.borrow().parent {
            Env::lookup(parent, name)
        } else {
            None
        }
    }

    pub fn evaluate(env: &Rc<RefCell<Env>>, expr: Rc<Value>) -> Result<Rc<Value>, String> {
        match &*expr {
            // FIXME: Can we avoid Rc::clone() here? I got the following error:
            // error[E0505]: cannot move out of `expr` because it is borrowed
            //     &Value::Null => Ok(expr),
            //                        ^^^^ move out of `expr` occurs here
            &Value::Null => Ok(Rc::clone(&expr)),
            &Value::Boolean(_) => Ok(Rc::clone(&expr)),
            &Value::Integer(_) => Ok(Rc::clone(&expr)),
            &Value::Symbol(ref name) =>
                Env::lookup(env, name).map(|var| var.borrow().value.clone())
                    .ok_or(format!("Not found: {}", name)),
            &Value::Pair(ref car, ref cdr) => {
                if let &Value::Symbol(ref name) = &**car {
                    if let Some(form) = forms::lookup(name) {
                        return form.apply(env, cdr.to_native_list()?.into_iter().collect());
                    }
                }
                let func = Env::evaluate(env, car.clone())?;
                let mut args = vec![];
                for expr in cdr.to_native_list()?.into_iter() {
                    args.push(Env::evaluate(env, expr)?);
                }
                if let &Value::Function(_, ref func) = &*func {
                    func.apply(args)
                } else {
                    Err("Not a function".to_string())
                }
            },
            _ => Err("Evaluate failed".to_string()),
        }
    }
}
