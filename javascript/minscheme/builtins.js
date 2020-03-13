const data = require('./data.js');

function print(value) {
  console.log(value.toString());
  return data.theUndef;
}

function unwrapInt(value) {
  if (!(value instanceof data.Int)) {
    throw new Error('not an int');
  }
  return value.rawValue;
}

function add(...args) {
  if (args.length === 0) {
    throw new Error('+ got 0 arg');
  }
  let a = 0;
  for (const arg of args) {
    a += unwrapInt(arg);
  }
  return new data.Int(a);
}

function sub(...args) {
  if (args.length === 0) {
    throw new Error('- got 0 arg');
  }
  let a = unwrapInt(args[0]);
  for (const arg of args.slice(1)) {
    a -= unwrapInt(arg);
  }
  return new data.Int(a);
}

function mul(...args) {
  if (args.length === 0) {
    throw new Error('* got 0 arg');
  }
  let a = 1;
  for (const arg of args) {
    a *= unwrapInt(arg);
  }
  return new data.Int(a);
}

function div(...args) {
  if (args.length === 0) {
    throw new Error('/ got 0 arg');
  }
  let a = unwrapInt(args[0]);
  for (const arg of args.slice(1)) {
    a /= unwrapInt(arg);
  }
  return new data.Int(a);
}

function eq(a, b) {
  return data.createBool(unwrapInt(a) === unwrapInt(b));
}

function lt(a, b) {
  return data.createBool(unwrapInt(a) < unwrapInt(b));
}

function lte(a, b) {
  return data.createBool(unwrapInt(a) <= unwrapInt(b));
}

function gt(a, b) {
  return data.createBool(unwrapInt(a) > unwrapInt(b));
}

function gte(a, b) {
  return data.createBool(unwrapInt(a) >= unwrapInt(b));
}

function and(...args) {
  let result = true;
  for (const arg of args) {
    result = result && !arg.equals(data.theFalse);
  }
  return data.createBool(result);
}

function or(...args) {
  let result = false;
  for (const arg of args) {
    result = result || !arg.equals(data.theFalse);
  }
  return data.createBool(result);
}

function not(value) {
  return data.createBool(value.equals(data.theFalse));
}

function eqCheck(a, b) {
  return data.createBool(a.equals(b));
}

function cons(car, cdr) {
  return new data.Pair(car, cdr);
}

function car(value) {
  if (!(value instanceof data.Pair)) {
    throw new Error('not a pair');
  }
  return value.car;
}

function cdr(value) {
  if (!(value instanceof data.Pair)) {
    throw new Error('not a pair');
  }
  return value.cdr;
}

const ALL = new Map();

function register(name, func) {
  ALL.set(name, new data.Func(name, func));
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

Object.assign(module.exports, {
  ALL,
});
