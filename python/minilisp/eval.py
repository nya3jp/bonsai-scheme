from minilisp import data
from minilisp import forms


def evaluate(env: data.Environment, expr: data.Value) -> data.Value:
    # print('DEBUG: evaluate: %s' % expr)
    if isinstance(expr, (data.Null, data.Bool, data.Int)):
        return expr
    if isinstance(expr, data.Symbol):
        return env.lookup(expr.name).value
    if isinstance(expr, data.Pair):
        if isinstance(expr.car, data.Symbol):
            name = expr.car.name
            form_func = forms.ALL_MAP.get(name)
            if form_func:
                args = data.to_native_list(expr.cdr)
                return form_func(env, args)
        func = evaluate(env, expr.car)
        args = [evaluate(env, value) for value in data.to_native_list(expr.cdr)]
        return func.apply(args)
    raise AssertionError('Can not evaluate: %s' % expr)
