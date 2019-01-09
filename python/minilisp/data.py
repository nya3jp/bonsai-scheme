import abc
from typing import Callable, Dict, List, Union, Optional


class Value(abc.ABC):
    def apply(self, args: List['Value']) -> 'Value':
        raise AssertionError('not function')

    @abc.abstractmethod
    def __str__(self) -> str:
        raise NotImplementedError()

    def __repr__(self) -> str:
        return str(self)


class BooleanValue(Value):
    raw_value: bool

    def __new__(cls, raw_value: bool):
        assert isinstance(raw_value, bool)
        try:
            return TRUE if raw_value else FALSE
        except NameError:
            value = super().__new__(cls)
            value.raw_value = raw_value
            return value

    def __str__(self) -> str:
        return '#t' if self.raw_value else '#f'


FALSE = BooleanValue(False)
TRUE = BooleanValue(True)


class IntegerValue(Value):
    raw_value: int

    def __init__(self, raw_value: int):
        self.raw_value = raw_value

    def __str__(self) -> str:
        return str(self.raw_value)


class SymbolValue(Value):
    _pool: Dict[str, 'SymbolValue'] = {}

    name: str

    def __new__(cls, name: str):
        pool = SymbolValue._pool
        if name not in pool:
            value = super().__new__(cls)
            value.name = name
            pool[name] = value
        return pool[name]

    def __str__(self) -> str:
        return self.name


class PairValue(Value):
    car: Value
    cdr: Value

    def __init__(self, car: Value, cdr: Value):
        self.car = car
        self.cdr = cdr

    def __str__(self) -> str:
        v = ['(', str(self.car)]
        cur = self.cdr
        while True:
            if isinstance(cur, NullValue):
                v.append(')')
                break
            elif isinstance(cur, PairValue):
                v.extend([' ', str(cur.car)])
                cur = cur.cdr
            else:
                v.extend([' . ', str(cur), ')'])
                break
        return ''.join(v)


class NullValue(Value):
    def __new__(cls):
        try:
            return NULL
        except NameError:
            return super().__new__(cls)

    def __str__(self) -> str:
        return '()'


NULL = NullValue()

ListValue = Union[NullValue, PairValue]


class UndefinedValue(Value):
    def __new__(cls):
        try:
            return UNDEF
        except NameError:
            return super().__new__(cls)

    def __str__(self) -> str:
        return '#undef'


UNDEF = UndefinedValue()


class FunctionValue(Value):
    name: str
    native_func: Callable[[List[Value]], Value]

    def __init__(self, name: str, native_func: Callable[[List[Value]], Value]):
        self.name = name
        self.native_func = native_func

    def apply(self, args: List[Value]) -> Value:
        return self.native_func(args)

    def __str__(self) -> str:
        return self.name


def to_native_list(list_value: ListValue) -> List[Value]:
    values = []
    while isinstance(list_value, PairValue):
        values.append(list_value.car)
        list_value = list_value.cdr
    assert list_value is NULL
    return values


def from_native_list(values: List[Value]) -> ListValue:
    list_value = NULL
    for value in reversed(values):
        list_value = PairValue(value, list_value)
    return list_value


class Variable:
    value: Value

    def __init__(self):
        self.value = UNDEF


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
