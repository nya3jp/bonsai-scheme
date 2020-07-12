use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use builtins;
use data::Value;
use data::ValueRef;
use error::Error;
use forms;

pub struct Variable {
    pub value: ValueRef,
}

impl Variable {
    pub fn new() -> Variable {
        Variable {
            value: ValueRef::new(Value::Undef),
        }
    }
}

impl Clone for Variable {
    fn clone(&self) -> Self {
        Variable {
            value: self.value.clone(),
        }
    }
}

pub struct Env {
    parent: Option<Rc<RefCell<Env>>>,
    vars: HashMap<String, Rc<RefCell<Variable>>>,
}

impl Env {
    pub fn new_top_level() -> Rc<RefCell<Env>> {
        let env = Env::new(None);
        builtins::install(&env);
        env
    }

    pub fn new(parent: Option<Rc<RefCell<Env>>>) -> Rc<RefCell<Env>> {
        Rc::new(RefCell::new(Env {
            parent: parent,
            vars: HashMap::new(),
        }))
    }

    pub fn ensure(env: &Rc<RefCell<Env>>, name: &str) -> Rc<RefCell<Variable>> {
        let mut env_ref = env.borrow_mut();
        if !env_ref.vars.contains_key(name) {
            env_ref
                .vars
                .insert(name.to_string(), Rc::new(RefCell::new(Variable::new())));
        }
        env_ref.vars.get(name).unwrap().clone()
    }

    pub fn lookup(env: &Rc<RefCell<Env>>, name: &str) -> Option<ValueRef> {
        Env::lookup_ref(env, name).map(|r| {
            let rr = r.borrow();
            rr.value.clone()
        })
    }

    pub fn lookup_ref(env: &Rc<RefCell<Env>>, name: &str) -> Option<Rc<RefCell<Variable>>> {
        if env.borrow().vars.contains_key(name) {
            Some(env.borrow().vars.get(name).unwrap().clone())
        } else if let Some(ref parent) = env.borrow().parent {
            Env::lookup_ref(parent, name)
        } else {
            None
        }
    }

    pub fn evaluate(env: &Rc<RefCell<Env>>, expr: &ValueRef) -> Result<ValueRef, Error> {
        let r = expr.borrow();
        match &*r {
            &Value::Null => Ok(expr.clone()),
            &Value::Boolean(_) => Ok(expr.clone()),
            &Value::Integer(_) => Ok(expr.clone()),
            &Value::Symbol(ref name) => {
                Env::lookup(env, name).ok_or(Error::new(format!("Not found: {}", name)))
            }
            &Value::Pair(ref car, ref cdr) => {
                {
                    let rr = car.borrow();
                    if let &Value::Symbol(ref name) = &*rr {
                        if let Some(form) = forms::lookup(name) {
                            return form.apply(env, cdr.to_native_list()?.as_slice());
                        }
                    }
                }
                let value = Env::evaluate(env, car)?;
                let (_, func) = value.as_function()?;
                let mut args = vec![];
                for expr in cdr.to_native_list()?.iter() {
                    args.push(Env::evaluate(env, expr)?);
                }
                func.apply(args.as_slice())
            }
            _ => Err(Error::new("Evaluate failed".into())),
        }
    }
}
