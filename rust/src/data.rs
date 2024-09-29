use std::{cell::RefCell, fmt, rc::Rc};

use anyhow::{bail, Result};

pub trait Function {
    fn apply(&self, args: &[Value]) -> Result<Value>;
}

impl<T: Fn(&[Value]) -> Result<Value>> Function for T {
    fn apply(&self, args: &[Value]) -> Result<Value> {
        self(args)
    }
}

#[derive(Clone)]
pub enum Value {
    Undef,
    Boolean(bool),
    Integer(i32),
    Symbol(Rc<String>),
    Null,
    Pair(Rc<ValueCell>, Rc<ValueCell>),
    Function(Rc<String>, Rc<dyn Function>),
}

impl Value {
    pub fn from_native_list(values: &[Value]) -> Value {
        values.iter().rfold(Value::Null, |list_value, elem_value| {
            Value::Pair(elem_value.clone().into(), list_value.into())
        })
    }

    pub fn to_native_list(&self) -> Result<Vec<Value>> {
        let mut values = vec![];
        let mut current = self.clone();
        loop {
            current = {
                match current {
                    Value::Null => break,
                    Value::Pair(car, cdr) => {
                        values.push(car.get());
                        cdr.get()
                    }
                    _ => bail!("Not a list"),
                }
            };
        }
        Ok(values)
    }

    pub fn bool(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }

    pub fn as_integer(&self) -> Result<i32> {
        let Value::Integer(i) = self else {
            bail!("Not an integer");
        };
        Ok(*i)
    }

    pub fn as_symbol(&self) -> Result<&str> {
        let Value::Symbol(name) = self else {
            bail!("Not a symbol");
        };
        Ok(name)
    }

    pub fn as_pair(&self) -> Result<(&ValueCell, &ValueCell)> {
        let Value::Pair(car, cdr) = self else {
            bail!("Not a pair");
        };
        Ok((car, cdr))
    }

    pub fn as_function(&self) -> Result<&dyn Function> {
        let Value::Function(_, func) = self else {
            bail!("Not a function");
        };
        Ok(&**func)
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Undef, Value::Undef) => true,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Null, Value::Null) => true,
            _ => false,
        }
    }
}

impl Eq for Value {}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Undef => write!(f, "#undef"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::Boolean(true) => write!(f, "#t"),
            Value::Integer(i) => write!(f, "{}", i),
            Value::Symbol(name) => write!(f, "{}", name),
            Value::Null => write!(f, "()"),
            Value::Pair(car, cdr) => write!(f, "({} . {})", car.get(), cdr.get()),
            Value::Function(name, _) => write!(f, "{}", name),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        (self as &dyn fmt::Display).fmt(f)
    }
}

pub struct ValueCell {
    cell: RefCell<Value>,
}

impl ValueCell {
    pub fn new(value: Value) -> Rc<Self> {
        Rc::new(Self {
            cell: RefCell::new(value),
        })
    }

    pub fn get(&self) -> Value {
        self.cell.borrow().clone()
    }

    pub fn set(&self, value: Value) {
        *self.cell.borrow_mut() = value
    }
}

impl From<Value> for Rc<ValueCell> {
    fn from(value: Value) -> Self {
        ValueCell::new(value)
    }
}
