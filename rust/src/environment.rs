use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use anyhow::anyhow;
use anyhow::bail;
use anyhow::Result;

use crate::builtins;
use crate::data::Value;
use crate::data::ValueRef;
use crate::forms;

pub struct Env {
    parent: Option<Rc<RefCell<Env>>>,
    vars: HashMap<String, ValueRef>,
}

impl Env {
    pub fn new_top_level() -> Rc<RefCell<Env>> {
        let env = Env::new(None);
        builtins::install(&env);
        env
    }

    pub fn new(parent: Option<Rc<RefCell<Env>>>) -> Rc<RefCell<Env>> {
        Rc::new(RefCell::new(Env {
            parent,
            vars: HashMap::new(),
        }))
    }

    pub fn ensure(env: &Rc<RefCell<Env>>, name: &str) -> ValueRef {
        let mut env_ref = env.borrow_mut();
        if !env_ref.vars.contains_key(name) {
            env_ref
                .vars
                .insert(name.to_string(), ValueRef::new(Value::Null));
        }
        env_ref.vars.get(name).unwrap().clone()
    }

    pub fn lookup(env: &Rc<RefCell<Env>>, name: &str) -> Option<Value> {
        Env::lookup_ref(env, name).map(|r| r.borrow().clone())
    }

    pub fn lookup_ref(env: &Rc<RefCell<Env>>, name: &str) -> Option<ValueRef> {
        if env.borrow().vars.contains_key(name) {
            Some(env.borrow().vars.get(name).unwrap().clone())
        } else if let Some(ref parent) = env.borrow().parent {
            Env::lookup_ref(parent, name)
        } else {
            None
        }
    }

    pub fn evaluate(env: &Rc<RefCell<Env>>, expr: &Value) -> Result<Value> {
        match expr {
            Value::Null => Ok(expr.clone()),
            Value::Boolean(_) => Ok(expr.clone()),
            Value::Integer(_) => Ok(expr.clone()),
            Value::Symbol(name) => Env::lookup(env, name).ok_or(anyhow!("Not found: {}", name)),
            Value::Pair(car, cdr) => {
                {
                    let rr = car.borrow();
                    if let Value::Symbol(name) = &*rr {
                        if let Some(form) = forms::lookup(name) {
                            return form.apply(env, cdr.borrow().to_native_list()?.as_slice());
                        }
                    }
                }
                let value = Env::evaluate(env, &*car.borrow())?;
                let (_, func) = value.as_function()?;
                let mut args = vec![];
                for expr in cdr.borrow().to_native_list()?.iter() {
                    args.push(Env::evaluate(env, expr)?);
                }
                func.apply(args.as_slice())
            }
            _ => bail!("Evaluate failed"),
        }
    }
}
