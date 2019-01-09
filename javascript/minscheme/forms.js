const data = require('./data.js');
const eval = require('./eval.js');

function quote(env, value) {
  return value;
}

function evalBody(env, exprs) {
  let result = data.theUndef;
  for (const expr of exprs) {
    result = eval.evaluate(env, expr)
  }
  return result;
}

function begin(env, ...rawArgs) {
  return evalBody(env, rawArgs);
}

const ALL = new Map();
ALL.set('quote', quote);
ALL.set('begin', begin);

Object.assign(module.exports, {
  ALL,
});
