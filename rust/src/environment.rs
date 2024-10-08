use std::{cell::RefCell, collections::HashMap, rc::Rc};

use anyhow::{anyhow, bail, Result};

use crate::{
    builtins,
    data::{Value, ValueCell},
    forms,
};

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
        match vars.get(name) {
            Some(cell) => cell.clone(),
            None => {
                let cell: Rc<ValueCell> = Value::Null.into();
                vars.insert(name.to_owned(), cell.clone());
                cell
            }
        }
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
            Value::Null | Value::Boolean(_) | Value::Integer(_) => Ok(expr.clone()),
            Value::Symbol(name) => self.lookup(name).ok_or(anyhow!("Not found: {}", name)),
            Value::Pair(car, cdr) => {
                if let Value::Symbol(name) = car.get() {
                    if let Some(form) = forms::lookup(&name) {
                        return form(self, &cdr.get().to_native_list()?);
                    }
                }
                let value = self.evaluate(&car.get())?;
                let func = value.as_function()?;
                let mut args = vec![];
                for expr in cdr.get().to_native_list()? {
                    args.push(self.evaluate(&expr)?);
                }
                func.apply(&args)
            }
            _ => bail!("Evaluate failed"),
        }
    }
}
