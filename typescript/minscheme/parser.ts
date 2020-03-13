import {arrayToValue, Int, Sym, theFalse, theTrue, Value} from './data';

const SKIP_RE = /^(\s+|;.*)+/;
const TOKEN_RE = /^[^\s);]+/;
const NUMBER_RE = /^-?[0-9]+$/;

function skip(code: string): string {
  const m = SKIP_RE.exec(code);
  if (!m) {
    return code;
  }
  return code.substring(m[0].length);
}

function createQuote(value: Value): Value {
  return arrayToValue([new Sym('quote'), value]);
}

function parsePartialValue(code: string): { value: Value; code: string } {
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
    return { value: arrayToValue(values), code: newRestCode.substring(1) };
  }

  const m = TOKEN_RE.exec(code);
  if (!m) {
    throw new Error('invalid token');
  }
  const token = m[0];
  let value;
  if (NUMBER_RE.test(token)) {
    const num = parseInt(token);
    value = new Int(num);
  } else if (token === '#f') {
    value = theFalse;
  } else if (token === '#t') {
    value = theTrue;
  } else {
    value = new Sym(token);
  }
  const newCode = code.substring(m.index + m[0].length);
  return { value, code: newCode };
}

function parsePartialList(code: string): { values: Value[]; code: string } {
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

export function parseValue(code: string): Value {
  const { value, code: restCode } = parsePartialValue(code);
  if (restCode !== '') {
    throw new Error('excess code');
  }
  return value;
}

export function parseList(code: string): Value[] {
  const { values, code: restCode } = parsePartialList(code);
  if (restCode !== '') {
    throw new Error('excess parentheses');
  }
  return values;
}
