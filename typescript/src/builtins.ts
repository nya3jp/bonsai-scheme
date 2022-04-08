import {
  Bool,
  createBool,
  Func,
  Int,
  Pair,
  theFalse,
  theUndef,
  Undef,
  Value,
} from './data';

function print(value: Value): Undef {
  console.log(value.toString());
  return theUndef;
}

function unwrapInt(value: Value): number {
  if (!(value instanceof Int)) {
    throw new Error('not an int');
  }
  return value.rawValue;
}

function add(...args: Value[]): Int {
  if (args.length === 0) {
    throw new Error('+ got 0 arg');
  }
  let a = 0;
  for (const arg of args) {
    a += unwrapInt(arg);
  }
  return new Int(a);
}

function sub(...args: Value[]): Int {
  if (args.length === 0) {
    throw new Error('- got 0 arg');
  }
  let a = unwrapInt(args[0]);
  for (const arg of args.slice(1)) {
    a -= unwrapInt(arg);
  }
  return new Int(a);
}

function mul(...args: Value[]): Int {
  if (args.length === 0) {
    throw new Error('* got 0 arg');
  }
  let a = 1;
  for (const arg of args) {
    a *= unwrapInt(arg);
  }
  return new Int(a);
}

function div(...args: Value[]): Int {
  if (args.length === 0) {
    throw new Error('/ got 0 arg');
  }
  let a = unwrapInt(args[0]);
  for (const arg of args.slice(1)) {
    a /= unwrapInt(arg);
  }
  return new Int(a);
}

function eq(a: Value, b: Value): Bool {
  return createBool(unwrapInt(a) === unwrapInt(b));
}

function lt(a: Value, b: Value): Bool {
  return createBool(unwrapInt(a) < unwrapInt(b));
}

function lte(a: Value, b: Value): Bool {
  return createBool(unwrapInt(a) <= unwrapInt(b));
}

function gt(a: Value, b: Value): Bool {
  return createBool(unwrapInt(a) > unwrapInt(b));
}

function gte(a: Value, b: Value): Bool {
  return createBool(unwrapInt(a) >= unwrapInt(b));
}

function and(...args: Value[]): Bool {
  let result = true;
  for (const arg of args) {
    result = result && !arg.equals(theFalse);
  }
  return createBool(result);
}

function or(...args: Value[]): Bool {
  let result = false;
  for (const arg of args) {
    result = result || !arg.equals(theFalse);
  }
  return createBool(result);
}

function not(value: Value): Bool {
  return createBool(value.equals(theFalse));
}

function eqCheck(a: Value, b: Value): Bool {
  return createBool(a.equals(b));
}

function cons(car: Value, cdr: Value): Pair {
  return new Pair(car, cdr);
}

function car(value: Value): Value {
  if (!(value instanceof Pair)) {
    throw new Error('not a pair');
  }
  return value.car;
}

function cdr(value: Value): Value {
  if (!(value instanceof Pair)) {
    throw new Error('not a pair');
  }
  return value.cdr;
}

export const allBuiltins = new Map<string, Func>();

function register(name: string, func: (...args: Value[]) => Value): void {
  allBuiltins.set(name, new Func(name, func));
}

register('print', print);
register('+', add);
register('-', sub);
register('*', mul);
register('/', div);
register('=', eq);
register('<', lt);
register('<=', lte);
register('>', gt);
register('>=', gte);
register('and', and);
register('or', or);
register('not', not);
register('eq?', eqCheck);
register('cons', cons);
register('car', car);
register('cdr', cdr);
