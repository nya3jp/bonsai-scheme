import {Environment, Func, Pair, Sym, Undef, Value, valueToArray} from './data';
import {evaluate} from './eval';

function quote(env: Environment, value: Value): Value {
  return value;
}

function evalBody(env: Environment, body: Value[]): Value {
  let result = Undef.theValue;
  for (const expr of body) {
    result = evaluate(env, expr);
  }
  return result;
}

function begin(env: Environment, ...body: Value[]): Value {
  return evalBody(env, body);
}

function makeLambdaFunc(
  env: Environment,
  name: string,
  rawParamsValue: Value,
  body: Value[]
): Value {
  const rawParams = valueToArray(rawParamsValue);
  const params: string[] = [];
  for (const rawParam of rawParams) {
    if (!(rawParam instanceof Sym)) {
      throw new Error('Non-symbol parameter');
    }
    params.push(rawParam.name);
  }
  return new Func(name, (...args: Value[]): Value => {
    const argsEnv = new Environment(env);
    if (args.length !== params.length) {
      throw new Error(
        `Function got ${args.length} arg(s); want ${params.length}`
      );
    }
    for (let i = 0; i < params.length; ++i) {
      argsEnv.ensure(params[i]).value = args[i];
    }
    return evalBody(argsEnv, body);
  });
}

function lambda(
  env: Environment,
  rawParamsValue: Value,
  ...body: Value[]
): Value {
  return makeLambdaFunc(env, '<lambda>', rawParamsValue, body);
}

function define(env: Environment, ...rawArgs: Value[]): Value {
  if (rawArgs.length === 0) {
    throw new Error('defined got 0 arg; want 1+');
  }
  const target = rawArgs[0];
  if (target instanceof Sym) {
    if (rawArgs.length !== 2) {
      throw new Error(`define got ${rawArgs.length} args; want 2`);
    }
    const value = evaluate(env, rawArgs[1]);
    env.ensure(target.name).value = value;
    return Undef.theValue;
  }
  if (!(target instanceof Pair)) {
    throw new Error('Malformed define: symbol or list required');
  }
  const sym = target.car;
  if (!(sym instanceof Sym)) {
    throw new Error('Malformed define: non-symbol name');
  }
  const name = sym.name;
  const value = makeLambdaFunc(env, name, target.cdr, rawArgs.slice(1));
  env.ensure(name).value = value;
  return Undef.theValue;
}

function ifs(
  env: Environment,
  rawTest: Value,
  rawThen: Value,
  rawElse: Value
): Value {
  const test = evaluate(env, rawTest);
  if (test.bool()) {
    return evaluate(env, rawThen);
  }
  return rawElse ? evaluate(env, rawElse) : Undef.theValue;
}

function cond(env: Environment, ...clauseValues: Value[]): Value {
  for (const clauseValue of clauseValues) {
    const clause = valueToArray(clauseValue);
    if (clause.length !== 2) {
      throw new Error('Malformed cond: 2-size lists expected');
    }
    const sym = clause[0];
    if (sym instanceof Sym && sym.name === 'else') {
      return evaluate(env, clause[1]);
    }
    const value = evaluate(env, clause[0]);
    if (value.bool()) {
      return evaluate(env, clause[1]);
    }
  }
  return Undef.theValue;
}

interface LetEnvFunc {
  (envs: {curEnv: Environment; origEnv: Environment}): {
    nextEnv: Environment;
    evalEnv: Environment;
  };
}

function letCommon(
  name: string,
  env: Environment,
  rawArgs: Value[],
  letEnvFunc: LetEnvFunc
): Value {
  const bindingValues = valueToArray(rawArgs[0]);
  let curEnv = new Environment(env);
  for (const bindingValue of bindingValues) {
    const binding = valueToArray(bindingValue);
    if (binding.length !== 2) {
      throw new Error(`Malformed ${name}: 2-size lists expected`);
    }
    const sym = binding[0];
    if (!(sym instanceof Sym)) {
      throw new Error(`Malformed ${name}: symbol expected`);
    }
    const {nextEnv, evalEnv} = letEnvFunc({curEnv, origEnv: env});
    const value = evaluate(evalEnv, binding[1]);
    curEnv = nextEnv;
    curEnv.ensure(sym.name).value = value;
  }
  return evalBody(curEnv, rawArgs.slice(1));
}

function letPlain(env: Environment, ...rawArgs: Value[]): Value {
  return letCommon(
    'let',
    env,
    rawArgs,
    ({curEnv, origEnv}: {curEnv: Environment; origEnv: Environment}) => {
      return {nextEnv: curEnv, evalEnv: origEnv};
    }
  );
}

function letStar(env: Environment, ...rawArgs: Value[]): Value {
  return letCommon(
    'let*',
    env,
    rawArgs,
    ({curEnv}: {curEnv: Environment; origEnv: Environment}) => {
      return {nextEnv: new Environment(curEnv), evalEnv: curEnv};
    }
  );
}

function letRec(env: Environment, ...rawArgs: Value[]): Value {
  return letCommon(
    'letrec',
    env,
    rawArgs,
    ({curEnv}: {curEnv: Environment; origEnv: Environment}) => {
      return {nextEnv: curEnv, evalEnv: curEnv};
    }
  );
}

function set(env: Environment, sym: Value, rawValue: Value): Value {
  if (!(sym instanceof Sym)) {
    throw new Error('Malformed set!: not a symbol');
  }
  const value = evaluate(env, rawValue);
  const v = env.lookup(sym.name);
  v.value = value;
  return Undef.theValue;
}

function setCar(env: Environment, rawPair: Value, rawValue: Value): Value {
  const pair = evaluate(env, rawPair);
  const value = evaluate(env, rawValue);
  if (!(pair instanceof Pair)) {
    throw new Error('set-car!: not a pair');
  }
  pair.car = value;
  return Undef.theValue;
}

function setCdr(env: Environment, rawPair: Value, rawValue: Value): Value {
  const pair = evaluate(env, rawPair);
  const value = evaluate(env, rawValue);
  if (!(pair instanceof Pair)) {
    throw new Error('set-cdr!: not a pair');
  }
  pair.cdr = value;
  return Undef.theValue;
}

export const allForms = new Map([
  ['quote', quote],
  ['begin', begin],
  ['lambda', lambda],
  ['define', define],
  ['if', ifs],
  ['cond', cond],
  ['let', letPlain],
  ['let*', letStar],
  ['letrec', letRec],
  ['set!', set],
  ['set-car!', setCar],
  ['set-cdr!', setCdr],
]);
