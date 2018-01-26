from typing import Dict, Optional

from minilisp import builtins
from minilisp import data
from minilisp import forms


class Variable:
    value: data.Value

    def __init__(self):
        self.value = data.UNDEF


class Environment:
    parent: Optional['Environment']
    vars: Dict[str, Variable]

    def __init__(self, parent: Optional['Environment']):
        self.parent = parent
        self.vars = {}

    def ensure(self, name: str) -> Variable:
        if name not in self.vars:
            self.vars[name] = Variable()
        return self.vars[name]

    def lookup(self, name: str) -> Variable:
        if name in self.vars:
            return self.vars[name]
        assert self.parent is not None, 'not found: %s' % name
        return self.parent.lookup(name)

    def evaluate(self, expr: data.Value) -> data.Value:
        # print('DEBUG: evaluate: %s' % expr)
        if isinstance(expr, (data.NullValue, data.BooleanValue, data.IntegerValue)):
            return expr
        if isinstance(expr, data.SymbolValue):
            return self.lookup(expr.name).value
        if isinstance(expr, data.PairValue):
            if isinstance(expr.car, data.SymbolValue):
                name = expr.car.name
                form_func = forms.ALL_MAP.get(name)
                if form_func:
                    args = data.to_native_list(expr.cdr)
                    return form_func(self, args)
            func = self.evaluate(expr.car)
            args = [self.evaluate(value) for value in data.to_native_list(expr.cdr)]
            return func.apply(args)
        raise AssertionError('Can not evaluate: %s' % expr)


def make_top_level_env() -> Environment:
    env = Environment(parent=None)
    for name, func in builtins.ALL_MAP.items():
        env.ensure(name).value = data.FunctionValue(name, func)
    return env
