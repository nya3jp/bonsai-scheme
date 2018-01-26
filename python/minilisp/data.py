import abc
from typing import Any, Callable, Dict, List, Union


class Value(abc.ABC):
    def boolean(self) -> bool:
        raise AssertionError('not boolean')

    def apply(self, args: List['Value']) -> 'Value':
        raise AssertionError('not function')

    @abc.abstractmethod
    def __eq__(self, other: Any) -> bool:
        raise NotImplementedError()

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

    def boolean(self) -> bool:
        return self.raw_value

    def __eq__(self, other: Any) -> bool:
        return (isinstance(other, BooleanValue) and
                other.raw_value == self.raw_value)

    def __str__(self) -> str:
        return '#t' if self.raw_value else '#f'


FALSE = BooleanValue(False)
TRUE = BooleanValue(True)


class IntegerValue(Value):
    raw_value: int

    def __init__(self, raw_value: int):
        self.raw_value = raw_value

    def __eq__(self, other: Any) -> bool:
        return (isinstance(other, IntegerValue) and
                other.raw_value == self.raw_value)

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

    def __eq__(self, other: Any) -> bool:
        return other is self

    def __str__(self) -> str:
        return self.name


class PairValue(Value):
    car: Value
    cdr: Value

    def __init__(self, car: Value, cdr: Value):
        self.car = car
        self.cdr = cdr

    def __eq__(self, other: Any) -> bool:
        return (isinstance(other, PairValue) and
                other.car == self.car and
                other.cdr == self.cdr)

    def __str__(self) -> str:
        # TODO: Prefer list styles.
        return '(%s . %s)' % (self.car, self.cdr)


class NullValue(Value):
    def __new__(cls):
        try:
            return NULL
        except NameError:
            return super().__new__(cls)

    def __eq__(self, other: Any) -> bool:
        return other is self

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

    def __eq__(self, other: Any) -> bool:
        return other is self

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

    def __eq__(self, other: Any) -> bool:
        return other is self

    def __str__(self) -> str:
        return '<func %s>' % self.name


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
