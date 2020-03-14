const data = require('./data.js');

const SKIP_RE = /^(\s+|;.*)+/;
const TOKEN_RE = /^[^\s);]+/;
const NUMBER_RE = /^-?[0-9]+$/;

function skip(code) {
  const m = SKIP_RE.exec(code);
  if (!m) {
    return code;
  }
  return code.substring(m[0].length);
}

function createQuote(value) {
  return data.arrayToValue([new data.Symbol('quote'), value]);
}

function parsePartialValue(code) {
  if (code.startsWith('\'')) {
    const { value, code: restCode } = parsePartialValue(code.substring(1));
    return { value: createQuote(value), code: restCode };
  }

  if (code.startsWith('(')) {
    const { values, code: restCode } = parsePartialList(code.substring(1));
    const newRestCode = skip(restCode);
    if (!newRestCode.startsWith(')')) {
      throw new Error('unexpected end of list');
    }
    return { value: data.arrayToValue(values), code: newRestCode.substring(1) };
  }

  const m = TOKEN_RE.exec(code);
  if (!m) {
    throw new Error('invalid token');
  }
  const token = m[0];
  let value;
  if (NUMBER_RE.test(token)) {
    const num = parseInt(token);
    value = new data.Int(num);
  } else if (token === '#f') {
    value = data.theFalse;
  } else if (token === '#t') {
    value = data.theTrue;
  } else {
    value = new data.Symbol(token);
  }
  const newCode = code.substring(m.index + m[0].length);
  return { value, code: newCode };
}

function parsePartialList(code) {
  const values = [];
  for (;;) {
    code = skip(code);
    if (code === '' || code.startsWith(')')) {
      break;
    }
    const { value, code: newCode } = parsePartialValue(code);
    values.push(value);
    code = newCode;
  }
  return { values, code };
}

function parseList(code) {
  const { values, code: restCode } = parsePartialList(code);
  if (restCode !== '') {
    throw new Error('excess parentheses');
  }
  return values;
}

Object.assign(module.exports, {
  parseList,
});
