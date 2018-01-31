use std::fmt;
use std::rc::Rc;

pub enum Value {
    Undef,
    Boolean(bool),
    Integer(i32),
    Symbol(String),
    Null,
    Pair(Rc<Value>, Rc<Value>),
}

impl Value {
    pub fn from_native_list(values: Vec<Rc<Value>>) -> Rc<Value> {
        let mut list_value = Rc::new(Value::Null);
        for value in values.into_iter().rev() {
            list_value = Rc::new(Value::Pair(value, list_value));
        }
        list_value
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
            &Value::Pair(ref car, ref cdr) => write!(f, "({} . {})", &*car, &*cdr),
        }
    }
}
