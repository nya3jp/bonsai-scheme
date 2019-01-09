from typing import Callable, Dict, List

from minscheme import data


def _builtin_print(args: List[data.Value]) -> data.Value:
    assert len(args) == 1
    value = args[0]
    print(str(value))
    return data.UNDEF


def _builtin_add(args: List[data.Value]) -> data.Value:
    assert args
    result = 0
    for value in args:
        assert isinstance(value, data.Int)
        result += value.raw_value
    return data.Int(result)


def _builtin_sub(args: List[data.Value]) -> data.Value:
    assert args
    first_value = args[0]
    assert isinstance(first_value, data.Int), 'not integer'
    result = first_value.raw_value
    for value in args[1:]:
        assert isinstance(value, data.Int), 'not integer'
        result -= value.raw_value
    return data.Int(result)


def _builtin_mul(args: List[data.Value]) -> data.Value:
    assert args
    result = 1
    for value in args:
        assert isinstance(value, data.Int), 'not integer'
        result *= value.raw_value
    return data.Int(result)


def _builtin_div(args: List[data.Value]) -> data.Value:
    assert args
    first_value = args[0]
    assert isinstance(first_value, data.Int), 'not integer'
    result = first_value.raw_value
    for value in args[1:]:
        assert isinstance(value, data.Int), 'not integer'
        result //= value.raw_value
    return data.Int(result)


def _builtin_eq(args: List[data.Value]) -> data.Value:
    assert len(args) == 2
    lhs, rhs = args
    assert isinstance(lhs, data.Int), 'not integer'
    assert isinstance(rhs, data.Int), 'not integer'
    return data.Bool(lhs.raw_value == rhs.raw_value)


def _builtin_lt(args: List[data.Value]) -> data.Value:
    assert len(args) == 2
    lhs, rhs = args
    assert isinstance(lhs, data.Int), 'not integer'
    assert isinstance(rhs, data.Int), 'not integer'
    return data.Bool(lhs.raw_value < rhs.raw_value)


def _builtin_lte(args: List[data.Value]) -> data.Value:
    assert len(args) == 2
    lhs, rhs = args
    assert isinstance(lhs, data.Int), 'not integer'
    assert isinstance(rhs, data.Int), 'not integer'
    return data.Bool(lhs.raw_value <= rhs.raw_value)


def _builtin_gt(args: List[data.Value]) -> data.Value:
    assert len(args) == 2
    lhs, rhs = args
    assert isinstance(lhs, data.Int), 'not integer'
    assert isinstance(rhs, data.Int), 'not integer'
    return data.Bool(lhs.raw_value > rhs.raw_value)


def _builtin_gte(args: List[data.Value]) -> data.Value:
    assert len(args) == 2
    lhs, rhs = args
    assert isinstance(lhs, data.Int), 'not integer'
    assert isinstance(rhs, data.Int), 'not integer'
    return data.Bool(lhs.raw_value >= rhs.raw_value)


def _builtin_and(args: List[data.Value]) -> data.Value:
    result = True
    for value in args:
        result = result and value != data.FALSE
    return data.Bool(result)


def _builtin_or(args: List[data.Value]) -> data.Value:
    result = False
    for value in args:
        result = result or value != data.FALSE
    return data.Bool(result)


def _builtin_not(args: List[data.Value]) -> data.Value:
    assert len(args) == 1
    value = args[0]
    return data.Bool(not (value != data.FALSE))


def _builtin_eq_check(args: List[data.Value]) -> data.Value:
    assert len(args) == 2
    lhs, rhs = args
    return data.Bool(lhs is rhs)


def _builtin_cons(args: List[data.Value]) -> data.Value:
    assert len(args) == 2
    return data.Pair(args[0], args[1])


def _builtin_car(args: List[data.Value]) -> data.Value:
    assert len(args) == 1
    value = args[0]
    assert isinstance(value, data.Pair), 'not pair'
    return value.car


def _builtin_cdr(args: List[data.Value]) -> data.Value:
    assert len(args) == 1
    value = args[0]
    assert isinstance(value, data.Pair), 'not pair'
    return value.cdr


ALL_MAP: Dict[str, Callable[[List[data.Value]], data.Value]] = {
    'print': _builtin_print,
    '+': _builtin_add,
    '-': _builtin_sub,
    '*': _builtin_mul,
    '/': _builtin_div,
    '=': _builtin_eq,
    '<': _builtin_lt,
    '<=': _builtin_lte,
    '>': _builtin_gt,
    '>=': _builtin_gte,
    'and': _builtin_and,
    'or': _builtin_or,
    'not': _builtin_not,
    'eq?': _builtin_eq_check,
    'cons': _builtin_cons,
    'car': _builtin_car,
    'cdr': _builtin_cdr,
}
