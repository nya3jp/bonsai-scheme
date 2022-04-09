export interface Value {
  toString(): string;
  equals(other: Value): boolean;
  bool(): boolean;
}

export class Undef implements Value {
  toString(): string {
    return '#undef';
  }

  equals(other: Value): boolean {
    return other instanceof Undef;
  }

  bool(): boolean {
    return true;
  }
}

export const theUndef = new Undef();

export class Null implements Value {
  toString(): string {
    return '()';
  }

  equals(other: Value): boolean {
    return other instanceof Null;
  }

  bool(): boolean {
    return true;
  }
}

export const theNull = new Null();

export class Pair implements Value {
  constructor(public car: Value, public cdr: Value) {}

  toString(): string {
    const v = ['(', this.car.toString()];
    let cur = this.cdr;
    while (cur) {
      if (cur instanceof Null) {
        v.push(')');
        break;
      } else if (cur instanceof Pair) {
        v.push(' ', cur.car.toString());
        cur = cur.cdr;
      } else {
        v.push(' . ', cur.toString(), ')');
        break;
      }
    }
    return v.join('');
  }

  equals(other: Value): boolean {
    return this === other;
  }

  bool(): boolean {
    return true;
  }
}

export function arrayToValue(array: Value[]): Value {
  let value = theNull;
  for (let i = array.length - 1; i >= 0; --i) {
    value = new Pair(array[i], value);
  }
  return value;
}

export function valueToArray(value: Value): Value[] {
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

export class Bool implements Value {
  constructor(public readonly rawValue: boolean) {}

  toString(): string {
    return this.rawValue ? '#t' : '#f';
  }

  equals(other: Value): boolean {
    if (!(other instanceof Bool)) {
      return false;
    }
    return this.rawValue === other.rawValue;
  }

  bool(): boolean {
    return this.rawValue;
  }
}

export const theFalse = new Bool(false);
export const theTrue = new Bool(true);

export function createBool(rawValue: boolean): Bool {
  return rawValue ? theTrue : theFalse;
}

export class Int implements Value {
  constructor(public readonly rawValue: number) {}

  toString(): string {
    return String(this.rawValue);
  }

  equals(other: Value): boolean {
    if (!(other instanceof Int)) {
      return false;
    }
    return this.rawValue === other.rawValue;
  }

  bool(): boolean {
    return true;
  }
}

export class Sym implements Value {
  constructor(public readonly name: string) {}

  toString(): string {
    return this.name;
  }

  equals(other: Value): boolean {
    if (!(other instanceof Sym)) {
      return false;
    }
    return this.name === other.name;
  }

  bool(): boolean {
    return true;
  }
}

export class Func implements Value {
  constructor(
    public readonly name: string,
    public readonly func: (...args: Value[]) => Value
  ) {}

  toString(): string {
    return this.name;
  }

  equals(other: Value): boolean {
    if (!(other instanceof Func)) {
      return false;
    }
    return this.func === other.func;
  }

  call(args: Value[]): Value {
    return this.func.apply(undefined, args);
  }

  bool(): boolean {
    return true;
  }
}

export class Variable {
  constructor(public value: Value) {}
}

export class Environment {
  public readonly vars = new Map<string, Variable>();

  constructor(public readonly parent?: Environment) {}

  ensure(name: string): Variable {
    let v = this.vars.get(name);
    if (v === undefined) {
      v = new Variable(theUndef);
      this.vars.set(name, v);
    }
    return v;
  }

  lookup(name: string): Variable {
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
