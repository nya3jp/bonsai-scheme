import {
  Bool,
  Environment,
  Func,
  Int,
  Pair, Sym,
  Undef,
  Value,
  valueToArray
} from './data';
import {allForms} from './forms';

export function evaluate(env: Environment, expr: Value): Value {
  if (expr instanceof Undef || expr instanceof Bool || expr instanceof Int) {
    return expr;
  }
  if (expr instanceof Sym) {
    return env.lookup(expr.name).value;
  }
  if (expr instanceof Pair) {
    const rawArgs = valueToArray(expr.cdr);
    if (expr.car instanceof Sym) {
      const form = allForms.get(expr.car.name);
      if (form) {
        return form(env, ...rawArgs);
      }
    }
    const func = evaluate(env, expr.car);
    if (!(func instanceof Func)) {
      throw new Error('can not call non-function value');
    }
    const args = rawArgs.map((rawArg) => evaluate(env, rawArg));
    return func.call(args);
  }
  throw new Error('not evaluatable value');
}
