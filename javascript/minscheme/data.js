class Value {
  toString() {
    throw new Error('abstract method');
  }

  equals(other) {
    throw new Error('abstract method');
  }

  bool() {
    throw new Error('abstract method');
  }
}

class Undef extends Value {
  toString() {
    return '#undef';
  }

  equals(other) {
    return other instanceof Undef;
  }

  bool() {
    return true;
  }
}

const theUndef = new Undef();

class Null extends Value {
  toString() {
    return '()';
  }

  equals(other) {
    return other instanceof Null;
  }

  bool() {
    return true;
  }
}

const theNull = new Null();

class Pair extends Value {
  constructor(car, cdr) {
    super();
    this.car = car;
    this.cdr = cdr;
  }

  toString() {
    const v = ['(', this.car.toString()];
    let cur = this.cdr;
    while (cur) {
      if (cur instanceof Null) {
        v.push(')');
        cur = null;
      } else if (cur instanceof Pair) {
        v.push(' ', cur.car.toString());
        cur = cur.cdr;
      } else {
        v.push(' . ', cur.toString(), ')');
        cur = null;
      }
    }
    return v.join('');
  }

  equals(other) {
    return this === other;
  }

  bool() {
    return true;
  }
}

function arrayToValue(array) {
  let value = theNull;
  for (let i = array.length - 1; i >= 0; --i) {
    value = new Pair(array[i], value);
  }
  return value;
}

function valueToArray(value) {
  const array = [];
  while (value instanceof Pair) {
    array.push(value.car);
    value = value.cdr;
  }
  if (!value.equals(theNull)) {
    throw new Error('not a list value');
  }
  return array;
}

class Bool extends Value {
  constructor(rawValue) {
    super();
    this.rawValue = rawValue;
  }

  toString() {
    return this.rawValue ? '#t' : '#f';
  }

  equals(other) {
    if (!(other instanceof Bool)) {
      return false;
    }
    return this.rawValue === other.rawValue;
  }

  bool() {
    return this.rawValue;
  }
}

const theFalse = new Bool(false);
const theTrue = new Bool(true);

function createBool(rawValue) {
  return rawValue ? theTrue : theFalse;
}

class Int extends Value {
  constructor(rawValue) {
    super();
    this.rawValue = rawValue;
  }

  toString() {
    return String(this.rawValue);
  }

  equals(other) {
    if (!(other instanceof Int)) {
      return false;
    }
    return this.rawValue === other.rawValue;
  }

  bool() {
    return true;
  }
}

class Symbol extends Value {
  constructor(name) {
    super();
    this.name = name;
  }

  toString() {
    return this.name;
  }

  equals(other) {
    if (!(other instanceof Symbol)) {
      return false;
    }
    return this.name === other.name;
  }

  bool() {
    return true;
  }
}

class Func extends Value {
  constructor(name, func) {
    super();
    this.name = name;
    this.func = func;
  }

  toString() {
    return this.name;
  }

  equals(other) {
    if (!(other instanceof Func)) {
      return false;
    }
    return this.func === other.func;
  }

  call(args) {
    return this.func.apply(undefined, args);
  }

  bool() {
    return true;
  }
}

class Variable {
  constructor(value) {
    this.value = value;
  }
}

class Environment {
  constructor(parent) {
    this.parent = parent;
    this.vars = new Map();
  }

  ensure(name) {
    let v = this.vars.get(name);
    if (v === undefined) {
      v = new Variable(theUndef);
      this.vars.set(name, v);
    }
    return v;
  }

  lookup(name) {
    const v = this.vars.get(name);
    if (v !== undefined) {
      return v;
    }
    if (!this.parent) {
      throw new Error('name not found: ' + name);
    }
    return this.parent.lookup(name);
  }
}

Object.assign(module.exports, {
  Undef,
  theUndef,
  Null,
  theNull,
  Pair,
  arrayToValue,
  valueToArray,
  Bool,
  theFalse,
  theTrue,
  createBool,
  Int,
  Symbol,
  Func,
  Environment,
});
