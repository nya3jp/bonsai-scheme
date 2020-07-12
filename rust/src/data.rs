use std::cell::Ref;
use std::cell::RefCell;
use std::cell::RefMut;
use std::fmt;
use std::rc::Rc;

use error::Error;

pub trait Function {
    fn apply(&self, args: &[ValueRef]) -> Result<ValueRef, Error>;
}

pub enum Value {
    Undef,
    Boolean(bool),
    Integer(i32),
    Symbol(String),
    Null,
    Pair(ValueRef, ValueRef),
    Function(String, Rc<dyn Function>),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (&Value::Undef, &Value::Undef) => true,
            (&Value::Boolean(a), &Value::Boolean(b)) => a == b,
            (&Value::Integer(a), &Value::Integer(b)) => a == b,
            (&Value::Symbol(ref a), &Value::Symbol(ref b)) => a == b,
            (&Value::Null, &Value::Null) => true,
            _ => false,
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
        (self as &dyn fmt::Display).fmt(f)
    }
}

pub struct ValueRef {
    pub r: Rc<RefCell<Value>>,
}

impl Clone for ValueRef {
    fn clone(&self) -> Self {
        ValueRef{r: self.r.clone()}
    }
}

impl ValueRef {
    pub fn new(value: Value) -> Self {
        ValueRef{r: Rc::new(RefCell::new(value))}
    }

    pub fn borrow(&self) -> Ref<Value> {
        self.r.borrow()
    }

    pub fn borrow_mut(&self) -> RefMut<Value> {
        self.r.borrow_mut()
    }

    pub fn from_native_list(values: &[ValueRef]) -> ValueRef {
        let mut list_value = ValueRef::new(Value::Null);
        for value in values.iter().rev() {
            list_value = ValueRef::new(Value::Pair(value.clone(), list_value));
        }
        list_value
    }

    pub fn to_native_list(&self) -> Result<Vec<ValueRef>, Error> {
        let mut values = vec![];
        let mut current = self.clone();
        loop {
            current = {
                let r = current.borrow();
                match &*r {
                    &Value::Null => break,
                    &Value::Pair(ref car, ref cdr) => {
                        values.push(car.clone());
                        cdr.clone()
                    },
                    _ => return Err(Error::new("Not a list".into())),
                }
            };
        }
        Ok(values)
    }

    pub fn bool(&self) -> bool {
        let r = self.borrow();
        if let &Value::Boolean(false) = &*r { false } else { true }
    }

    pub fn as_integer(&self) -> Result<i32, Error> {
        let r = self.borrow();
        if let &Value::Integer(i) = &*r {
            Ok(i)
        } else {
            Err(Error::new("Not an integer".into()))
        }
    }

    pub fn as_symbol(&self) -> Result<String, Error> {
        let r = self.borrow();
        if let &Value::Symbol(ref name) = &*r {
            Ok(name.clone())
        } else {
            Err(Error::new("Not a symbol".into()))
        }
    }

    pub fn as_pair(&self) -> Result<(ValueRef, ValueRef), Error> {
        let r = self.borrow();
        if let &Value::Pair(ref car, ref cdr) = &*r {
            Ok((car.clone(), cdr.clone()))
        } else {
            Err(Error::new("Not a pair".into()))
        }
    }

    pub fn as_function(&self) -> Result<(String, Rc<dyn Function>), Error> {
        let r = self.borrow();
        if let &Value::Function(ref name, ref func) = &*r {
            Ok((name.clone(), func.clone()))
        } else {
            Err(Error::new("Not a function".into()))
        }
    }
}

impl PartialEq for ValueRef {
    fn eq(&self, other: &Self) -> bool {
        let rself = self.r.borrow();
        let rother = other.r.borrow();
        rself.eq(&*rother)
    }
}

impl fmt::Display for ValueRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let r = self.r.borrow();
        r.fmt(f)
    }
}

impl fmt::Debug for ValueRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let r = self.r.borrow();
        r.fmt(f)
    }
}
