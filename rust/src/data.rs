use std::fmt;
use std::rc::Rc;

pub trait Function {
    fn apply(&self, args: Vec<Rc<Value>>) -> Result<Rc<Value>, String>;
}

pub enum Value {
    Undef,
    Boolean(bool),
    Integer(i32),
    Symbol(String),
    Null,
    Pair(Rc<Value>, Rc<Value>),
    Function(String, Box<Function>),
}

impl Value {
    pub fn from_native_list(values: Vec<Rc<Value>>) -> Rc<Value> {
        let mut list_value = Rc::new(Value::Null);
        for value in values.into_iter().rev() {
            list_value = Rc::new(Value::Pair(value, list_value));
        }
        list_value
    }

    pub fn to_native_list(&self) -> Result<Vec<Rc<Value>>, String> {
        let mut values = vec![];
        let mut current: &Value = self;
        loop {
            match current {
                &Value::Null => break,
                &Value::Pair(ref car, ref cdr) => {
                    values.push(car.clone());
                    current = &**cdr;
                },
                _ => return Err("Not a list".to_string()),
            };
        }
        Ok(values)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Value::Undef => write!(f, "#undef"),
            &Value::Boolean(false) => write!(f, "#f"),
            &Value::Boolean(true) => write!(f, "#t"),
            &Value::Integer(i) => write!(f, "{}", i),
            &Value::Symbol(ref name) => write!(f, "{}", name),
            &Value::Null => write!(f, "()"),
            &Value::Pair(ref car, ref cdr) => write!(f, "({} . {})", car, cdr),
            &Value::Function(ref name, _) => write!(f, "{}", name),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        (self as &fmt::Display).fmt(f);
        Ok(())
    }
}
