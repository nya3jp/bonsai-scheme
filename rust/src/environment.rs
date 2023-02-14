use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use anyhow::anyhow;
use anyhow::bail;
use anyhow::Result;

use crate::builtins;
use crate::data::Value;
use crate::data::ValueCell;
use crate::forms;

pub struct Env {
    parent: Option<Rc<Env>>,
    vars: RefCell<HashMap<String, Rc<ValueCell>>>,
}

impl Env {
    pub fn new_top_level() -> Rc<Self> {
        let env = Env::new(None);
        builtins::install(&env);
        env
    }

    pub fn new(parent: Option<Rc<Self>>) -> Rc<Self> {
        Rc::new(Env {
            parent,
            vars: Default::default(),
        })
    }

    pub fn ensure(self: &Rc<Self>, name: &str) -> Rc<ValueCell> {
        let mut vars = self.vars.borrow_mut();
        if !vars.contains_key(name) {
            vars.insert(name.to_owned(), Value::Null.into());
        }
        vars.get(name).unwrap().clone()
    }

    pub fn lookup(self: &Rc<Self>, name: &str) -> Option<Value> {
        self.lookup_ref(name).map(|r| r.get())
    }

    pub fn lookup_ref(self: &Rc<Self>, name: &str) -> Option<Rc<ValueCell>> {
        {
            let vars = self.vars.borrow();
            if let Some(r) = vars.get(name) {
                return Some(r.clone());
            }
        }
        if let Some(parent) = &self.parent {
            return parent.lookup_ref(name);
        }
        None
    }

    pub fn evaluate(self: &Rc<Self>, expr: &Value) -> Result<Value> {
        match expr {
            Value::Null => Ok(expr.clone()),
            Value::Boolean(_) => Ok(expr.clone()),
            Value::Integer(_) => Ok(expr.clone()),
            Value::Symbol(name) => self.lookup(name).ok_or(anyhow!("Not found: {}", name)),
            Value::Pair(car, cdr) => {
                if let Value::Symbol(name) = car.get() {
                    if let Some(form) = forms::lookup(&name) {
                        return form(self, cdr.get().to_native_list()?.as_slice());
                    }
                }
                let value = self.evaluate(&car.get())?;
                let func = value.as_function()?;
                let mut args = vec![];
                for expr in cdr.get().to_native_list()? {
                    args.push(self.evaluate(&expr)?);
                }
                func.apply(args.as_slice())
            }
            _ => bail!("Evaluate failed"),
        }
    }
}
