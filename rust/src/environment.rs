use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use builtins;
use data::Value;

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
    pub fn new_top_level() -> Env {
        let mut env = Env::new(None);
        builtins::install(&mut env);
        env
    }

    pub fn new(parent: Option<Rc<RefCell<Env>>>) -> Env {
        Env{parent: parent, vars: HashMap::new()}
    }

    pub fn ensure(&mut self, name: &String) -> Rc<RefCell<Variable>> {
        if !self.vars.contains_key(name) {
            self.vars.insert(name.clone(), Rc::new(RefCell::new(Variable::new())));
        }
        self.vars.get(name).unwrap().clone()
    }

    pub fn lookup(&self, name: &String) -> Option<Rc<RefCell<Variable>>> {
        if self.vars.contains_key(name) {
            Some(self.vars.get(name).unwrap().clone())
        } else if let Some(ref parent) = self.parent {
            parent.borrow().lookup(name)
        } else {
            None
        }
    }

    pub fn evaluate(&self, expr: Rc<Value>) -> Result<Rc<Value>, String> {
        match &*expr {
            // FIXME: Can we avoid Rc::clone() here? I got the following error:
            // error[E0505]: cannot move out of `expr` because it is borrowed
            //     &Value::Null => Ok(expr),
            //                        ^^^^ move out of `expr` occurs here
            &Value::Null => Ok(Rc::clone(&expr)),
            &Value::Boolean(_) => Ok(Rc::clone(&expr)),
            &Value::Integer(_) => Ok(Rc::clone(&expr)),
            &Value::Symbol(ref name) =>
                self.lookup(name).map(|var| var.borrow().value.clone())
                    .ok_or(format!("Not found: {}", name)),
            &Value::Pair(ref car, ref cdr) => {
                let func = self.evaluate(car.clone())?;
                let mut args = vec![];
                for expr in cdr.to_native_list()?.into_iter() {
                    args.push(self.evaluate(expr)?);
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
