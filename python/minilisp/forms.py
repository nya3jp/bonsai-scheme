from typing import Callable, Dict, List

from minilisp import data
from minilisp import environments


def _evaluate_body(env: 'environments.Environment', body: List[data.Value]) -> data.Value:
    result = data.UNDEF
    for expr in body:
        result = env.evaluate(expr)
    return result


def _form_quote(env: 'environments.Environment', args: List[data.Value]) -> data.Value:
    del env  # unused
    assert len(args) == 1
    value = args[0]
    return value


def _form_begin(env: 'environments.Environment', args: List[data.Value]) -> data.Value:
    return _evaluate_body(env, args)


def _make_func_value(env: 'environments.Environment', name: str, params_value: data.Value, body: List[data.Value]) -> data.FunctionValue:
    # TODO: support variable args
    params = []
    for value in data.to_native_list(params_value):
        assert isinstance(value, data.SymbolValue)
        params.append(value.name)

    def func_impl(args: List[data.Value]) -> data.Value:
        # TODO: handle internal defines properly
        assert len(args) == len(params), 'arg count mismatch'
        func_env = environments.Environment(parent=env)
        for param, arg in zip(params, args):
            func_env.ensure(param).value = arg
        return _evaluate_body(func_env, body)

    return data.FunctionValue(name, func_impl)


def _form_lambda(env: 'environments.Environment', args: List[data.Value]) -> data.Value:
    assert len(args) >= 2
    return _make_func_value(env, '<lambda>', args[0], args[1:])


def _form_define(env: 'environments.Environment', args: List[data.Value]) -> data.Value:
    assert len(args) >= 2

    target = args[0]
    if isinstance(target, data.SymbolValue):
        assert len(args) == 2
        name = target.name
        value = env.evaluate(args[1])
        env.ensure(name).value = value
        return data.UNDEF

    assert isinstance(target, data.PairValue)
    assert isinstance(target.car, data.SymbolValue)

    name = target.car.name
    func_value = _make_func_value(env, name, target.cdr, args[1:])
    env.ensure(name).value = func_value

    return data.UNDEF


def _form_set(env: 'environments.Environment', args: List[data.Value]) -> data.Value:
    assert len(args) == 2
    target = args[0]
    assert isinstance(target, data.SymbolValue)
    name = target.name
    value = env.evaluate(args[1])
    env.lookup(name).value = value
    return data.UNDEF


def _form_if(env: 'environments.Environment', args: List[data.Value]) -> data.Value:
    assert len(args) in (2, 3)

    test_expr, true_expr = args[:2]
    false_expr = args[2] if len(args) == 3 else data.UNDEF

    test_value = env.evaluate(test_expr)
    return env.evaluate(true_expr if test_value is not data.FALSE else false_expr)


def _form_cond(env: 'environments.Environment', args: List[data.Value]) -> data.Value:
    clauses = [data.to_native_list(arg) for arg in args]
    for clause in clauses:
        if (clause[0] == data.SymbolValue('else') or
                env.evaluate(clause[0]) is not data.FALSE):
            return _evaluate_body(env, clause[1:])
    return data.UNDEF


def _form_let(env: 'environments.Environment', args: List[data.Value]) -> data.Value:
    assert len(args) >= 1
    let_env = environments.Environment(parent=env)
    for pair_expr in data.to_native_list(args[0]):
        pair = data.to_native_list(pair_expr)
        assert len(pair) == 2
        target, expr = pair
        assert isinstance(target, data.SymbolValue)
        let_env.ensure(target.name).value = env.evaluate(expr)
    return _evaluate_body(let_env, args[1:])


def _form_let_star(env: 'environments.Environment', args: List[data.Value]) -> data.Value:
    assert len(args) >= 1
    let_env = environments.Environment(parent=env)
    for pair_expr in data.to_native_list(args[0]):
        pair = data.to_native_list(pair_expr)
        assert len(pair) == 2
        target, expr = pair
        assert isinstance(target, data.SymbolValue)
        let_env.ensure(target.name).value = let_env.evaluate(expr)
    return _evaluate_body(let_env, args[1:])


ALL_MAP: Dict[str, Callable[['environments.Environment', List[data.Value]], data.Value]] = {
    'quote': _form_quote,
    'begin': _form_begin,
    'lambda': _form_lambda,
    'define': _form_define,
    'if': _form_if,
    'cond': _form_cond,
    'let': _form_let,
    'let*': _form_let_star,
    'set!': _form_set,
}
