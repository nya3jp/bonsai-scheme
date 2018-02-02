use std::fmt;
use std::rc::Rc;

pub trait Function {
    fn apply(&self, args: &[Rc<Value>]) -> Result<Rc<Value>, String>;
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
    pub fn from_native_list(values: &[Rc<Value>]) -> Rc<Value> {
        let mut list_value = Rc::new(Value::Null);
        for value in values.iter().rev() {
            list_value = Rc::new(Value::Pair(value.clone(), list_value));
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

    pub fn bool(&self) -> bool {
        if let &Value::Boolean(false) = self { false } else { true }
    }

    pub fn as_integer(&self) -> Result<i32, String> {
        if let &Value::Integer(i) = self {
            Ok(i)
        } else {
            Err("Not an integer".to_string())
        }
    }

    pub fn as_symbol(&self) -> Result<String, String> {
        if let &Value::Symbol(ref name) = self {
            Ok(name.clone())
        } else {
            Err("Not a symbol".to_string())
        }
    }

    pub fn as_pair(&self) -> Result<(Rc<Value>, Rc<Value>), String> {
        if let &Value::Pair(ref car, ref cdr) = self {
            Ok((car.clone(), cdr.clone()))
        } else {
            Err("Not a pair".to_string())
        }
    }

    pub fn as_function(&self) -> Result<(String, &Function), String> {
        if let &Value::Function(ref name, ref func) = self {
            Ok((name.clone(), &**func))
        } else {
            Err("Not a function".to_string())
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        // FIXME: Can we write this in more readable way?
        match self {
            &Value::Undef => if let &Value::Undef = other { true } else { false },
            &Value::Boolean(a) => if let &Value::Boolean(b) = other { a == b } else { false },
            &Value::Integer(a) => if let &Value::Integer(b) = other { a == b } else { false },
            &Value::Symbol(ref a) => if let &Value::Symbol(ref b) = other { a == b } else { false },
            // FIXME: This is wrong.
            &Value::Pair(_, _) => false,
            &Value::Null => if let &Value::Null = other { true } else { false },
            // FIXME: This is wrong.
            &Value::Function(_, _) => false,
        }
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
