import re
from typing import List, Tuple

from minscheme import data

_SKIP_RE = re.compile(r'^(\s+|;.*)+')
_TOKEN_RE = re.compile(r'^[^\s);]+')
_NUM_RE = re.compile(r'^-?[0-9]+$')


class ParseError(Exception):
    pass


def _make_quote(value: data.Value) -> data.Pair:
    return data.from_native_list([data.Symbol('quote'), value])


def _parse_skip(code: str) -> str:
    return _SKIP_RE.sub('', code)


def _parse_value(code: str) -> Tuple[data.Value, str]:
    if code.startswith('\''):
        value, rest_code = _parse_value(code[1:])
        return _make_quote(value), rest_code

    if code.startswith('('):
        values, rest_code = _parse_list(code[1:])
        rest_code = _parse_skip(rest_code)
        if not rest_code.startswith(')'):
            raise ParseError(rest_code)
        return data.from_native_list(values), rest_code[1:]

    match = _TOKEN_RE.search(code)
    if not match:
        raise ParseError(code)
    token = match.group()
    if _NUM_RE.search(token):
        value = data.Int(int(token))
    elif token == '#f':
        value = data.Bool(False)
    elif token == '#t':
        value = data.Bool(True)
    else:
        value = data.Symbol(token)
    return value, code[match.end():]


def _parse_list(code: str) -> Tuple[List[data.Value], str]:
    values = []
    while True:
        code = _parse_skip(code)
        if not code or code.startswith(')'):
            break
        value, code = _parse_value(code)
        values.append(value)
    return values, code


def parse(code: str) -> List[data.Value]:
    values, excess_code = _parse_list(code)
    if excess_code:
        raise ParseError(excess_code)
    return values
