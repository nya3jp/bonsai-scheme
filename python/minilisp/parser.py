import re
from typing import List, Tuple

from minilisp import data

_SKIP_RE = re.compile(r'^(\s+|;.*)+')
_TOKEN_RE = re.compile(r'^[^\s);]+')
_NUM_RE = re.compile(r'^-?[0-9]+$')


class ParseError(Exception):
    pass


def _make_quote(value: data.Value) -> data.PairValue:
    return data.from_native_list([data.SymbolValue('quote'), value])


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
        value = data.IntegerValue(int(token))
    elif token == '#f':
        value = data.BooleanValue(False)
    elif token == '#t':
        value = data.BooleanValue(True)
    else:
        value = data.SymbolValue(token)
    return value, code[match.end():]


def _parse_list(code: str) -> Tuple[List[data.Value], str]:
    values = []
    while True:
        code = _parse_skip(code)
        if not code or code.startswith(')'):
            break
        # TODO: Support dots.
        value, code = _parse_value(code)
        values.append(value)
    return values, code


def parse(code: str) -> List[data.Value]:
    values, excess_code = _parse_list(code)
    if excess_code:
        raise ParseError(excess_code)
    return values
