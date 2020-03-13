const data = require('./data.js');
const eval = require('./eval.js');

function quote(env, value) {
  return value;
}

function evalBody(env, body) {
  let result = data.theUndef;
  for (const expr of body) {
    result = eval.evaluate(env, expr)
  }
  return result;
}

function begin(env, ...body) {
  return evalBody(env, body);
}

function makeLambdaFunc(env, name, rawParamsValue, body) {
  const rawParams = data.valueToArray(rawParamsValue);
  const params = [];
  for (const rawParam of rawParams) {
    if (!rawParam instanceof data.Symbol) {
      throw new Error('Non-symbol parameter');
    }
    params.push(rawParam.name);
  }
  return new data.Func(name, function(...args) {
    const argsEnv = new data.Environment(env);
    if (args.length !== params.length) {
      throw new Error(`Function got ${args.length} arg(s); want ${params.length}`);
    }
    for (let i = 0; i < params.length; ++i) {
      argsEnv.ensure(params[i]).value = args[i];
    }
    return evalBody(argsEnv, body);
  });
}

function lambda(env, rawParamsValue, ...body) {
  return makeLambdaFunc(env, '<lambda>', rawParamsValue, body)
}

function define(env, ...rawArgs) {
  if (rawArgs.length === 0) {
    throw new Error('defined got 0 arg; want 1+');
  }
  const target = rawArgs[0];
  if (target instanceof data.Symbol) {
    if (rawArgs.length !== 2) {
      throw new Error(`define got ${rawArgs.length} args; want 2`);
    }
    const value = eval.evaluate(env, rawArgs[1]);
    env.ensure(target.name).value = value;
    return data.theUndef;
  }
  if (!target instanceof data.Pair) {
    throw new Error('Malformed define: symbol or list required');
  }
  const sym = target.car;
  if (!sym instanceof data.Symbol) {
    throw new Error('Malformed define: non-symbol name');
  }
  const name = sym.name;
  const value = makeLambdaFunc(env, name, target.cdr, rawArgs.slice(1));
  env.ensure(name).value = value;
  return data.theUndef;
}

function ifs(env, rawTest, rawThen, rawElse) {
  const test = eval.evaluate(env, rawTest);
  if (test.bool()) {
    return eval.evaluate(env, rawThen);
  }
  return rawElse ? eval.evaluate(env, rawElse) : data.theUndef;
}

function cond(env, ...clauseValues) {
  for (const clauseValue of clauseValues) {
    const clause = data.valueToArray(clauseValue);
    if (clause.length !== 2) {
      throw new Error('Malformed cond: 2-size lists expected');
    }
    const sym = clause[0];
    if (sym instanceof data.Symbol && sym.name === 'else') {
      return eval.evaluate(env, clause[1]);
    }
    const value = eval.evaluate(env, clause[0]);
    if (value.bool()) {
      return eval.evaluate(env, clause[1]);
    }
  }
  return data.theUndef;
}

const ALL = new Map();
ALL.set('quote', quote);
ALL.set('begin', begin);
ALL.set('lambda', lambda);
ALL.set('define', define);
ALL.set('if', ifs);
ALL.set('cond', cond);

Object.assign(module.exports, {
  ALL,
});
