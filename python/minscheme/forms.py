from typing import Callable, Dict, List

from minscheme import data
from minscheme import eval


def _evaluate_body(env: data.Environment, body: List[data.Value]) -> data.Value:
    result = data.UNDEF
    for expr in body:
        result = eval.evaluate(env, expr)
    return result


def _form_quote(env: data.Environment, args: List[data.Value]) -> data.Value:
    del env  # unused
    assert len(args) == 1
    value = args[0]
    return value


def _form_begin(env: data.Environment, args: List[data.Value]) -> data.Value:
    return _evaluate_body(env, args)


def _make_func_value(env: data.Environment, name: str, params_value: data.Value, body: List[data.Value]) -> data.Func:
    params = []
    for value in data.to_native_list(params_value):
        assert isinstance(value, data.Symbol)
        params.append(value.name)

    def func_impl(args: List[data.Value]) -> data.Value:
        # TODO: handle internal defines properly
        assert len(args) == len(params), 'arg count mismatch'
        func_env = data.Environment(parent=env)
        for param, arg in zip(params, args):
            func_env.ensure(param).value = arg
        return _evaluate_body(func_env, body)

    return data.Func(name, func_impl)


def _form_lambda(env: data.Environment, args: List[data.Value]) -> data.Value:
    assert len(args) >= 2
    return _make_func_value(env, '<lambda>', args[0], args[1:])


def _form_define(env: data.Environment, args: List[data.Value]) -> data.Value:
    assert len(args) >= 2

    target = args[0]
    if isinstance(target, data.Symbol):
        assert len(args) == 2
        name = target.name
        value = eval.evaluate(env, args[1])
        env.ensure(name).value = value
        return data.UNDEF

    assert isinstance(target, data.Pair)
    assert isinstance(target.car, data.Symbol)

    name = target.car.name
    func_value = _make_func_value(env, name, target.cdr, args[1:])
    env.ensure(name).value = func_value

    return data.UNDEF


def _form_if(env: data.Environment, args: List[data.Value]) -> data.Value:
    assert len(args) in (2, 3)

    test_expr, true_expr = args[:2]
    false_expr = args[2] if len(args) == 3 else data.UNDEF

    test_value = eval.evaluate(env, test_expr)
    return eval.evaluate(env, true_expr if test_value is not data.FALSE else false_expr)


def _form_cond(env: data.Environment, args: List[data.Value]) -> data.Value:
    clauses = [data.to_native_list(arg) for arg in args]
    for clause in clauses:
        if (clause[0] == data.Symbol('else') or
                eval.evaluate(env, clause[0]) is not data.FALSE):
            return _evaluate_body(env, clause[1:])
    return data.UNDEF


def _form_let(env: data.Environment, args: List[data.Value]) -> data.Value:
    assert len(args) >= 1
    let_env = data.Environment(parent=env)
    for pair_expr in data.to_native_list(args[0]):
        pair = data.to_native_list(pair_expr)
        assert len(pair) == 2
        target, expr = pair
        assert isinstance(target, data.Symbol)
        let_env.ensure(target.name).value = eval.evaluate(env, expr)
    return _evaluate_body(let_env, args[1:])


def _form_let_star(env: data.Environment, args: List[data.Value]) -> data.Value:
    assert len(args) >= 1
    let_env = env
    for pair_expr in data.to_native_list(args[0]):
        pair = data.to_native_list(pair_expr)
        assert len(pair) == 2
        target, expr = pair
        assert isinstance(target, data.Symbol)
        parent_env = let_env
        let_env = data.Environment(parent=parent_env)
        let_env.ensure(target.name).value = eval.evaluate(parent_env, expr)
    return _evaluate_body(let_env, args[1:])


def _form_letrec(env: data.Environment, args: List[data.Value]) -> data.Value:
    assert len(args) >= 1
    let_env = data.Environment(parent=env)
    for pair_expr in data.to_native_list(args[0]):
        pair = data.to_native_list(pair_expr)
        assert len(pair) == 2
        target, expr = pair
        assert isinstance(target, data.Symbol)
        let_env.ensure(target.name).value = eval.evaluate(let_env, expr)
    return _evaluate_body(let_env, args[1:])


def _form_set(env: data.Environment, args: List[data.Value]) -> data.Value:
    assert len(args) == 2
    target = args[0]
    assert isinstance(target, data.Symbol)
    name = target.name
    value = eval.evaluate(env, args[1])
    env.lookup(name).value = value
    return data.UNDEF


def _form_set_car(env: data.Environment, args: List[data.Value]) -> data.Value:
    assert len(args) == 2
    pair = eval.evaluate(env, args[0])
    assert isinstance(pair, data.Pair)
    value = eval.evaluate(env, args[1])
    pair.car = value
    return data.UNDEF


def _form_set_cdr(env: data.Environment, args: List[data.Value]) -> data.Value:
    assert len(args) == 2
    pair = eval.evaluate(env, args[0])
    assert isinstance(pair, data.Pair)
    value = eval.evaluate(env, args[1])
    pair.cdr = value
    return data.UNDEF


ALL_MAP: Dict[str, Callable[[data.Environment, List[data.Value]], data.Value]] = {
    'quote': _form_quote,
    'begin': _form_begin,
    'lambda': _form_lambda,
    'define': _form_define,
    'if': _form_if,
    'cond': _form_cond,
    'let': _form_let,
    'let*': _form_let_star,
    'letrec': _form_letrec,
    'set!': _form_set,
    'set-car!': _form_set_car,
    'set-cdr!': _form_set_cdr,
}
