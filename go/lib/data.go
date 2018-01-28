package lib

import "fmt"

type Value interface {
	fmt.Stringer
	Boolean() bool
}

type Undef struct{}

var TheUndef = &Undef{}

func (value *Undef) String() string {
	return "#undef"
}

func (value *Undef) Boolean() bool {
	panic("not boolean")
}

type ListValue interface {
	Value
	ToSlice() []Value
}

type Null struct{}

var TheNull = &Null{}

func (null *Null) String() string {
	return "()"
}

func (null *Null) Boolean() bool {
	panic("not boolean")
}

func (null *Null) ToSlice() []Value {
	return make([]Value, 0)
}

type Pair struct {
	Car Value
	Cdr Value
}

func (pair *Pair) String() string {
	return fmt.Sprintf("(%v . %v)", pair.Car, pair.Cdr)
}

func (pair *Pair) Boolean() bool {
	panic("not boolean")
}

func (pair *Pair) ToSlice() []Value {
	slice := []Value{}
	var current Value = pair
	for {
		if _, ok := current.(*Null); ok {
			break
		} else if pair, ok := current.(*Pair); ok {
			slice = append(slice, pair.Car)
			current = pair.Cdr
		} else {
			panic("not list")
		}
	}
	return slice
}

func ToListValue(slice []Value) ListValue {
	if len(slice) == 0 {
		return TheNull
	}
	return &Pair{slice[0], ToListValue(slice[1:])}
}

type Boolean struct {
	RawValue bool
}

var (
	TheFalse = &Boolean{false}
	TheTrue  = &Boolean{true}
)

func (value *Boolean) String() string {
	if value.RawValue {
		return "#t"
	} else {
		return "#f"
	}
}

func (value *Boolean) Boolean() bool {
	return value.RawValue
}

func MakeBoolean(rawValue bool) *Boolean {
	if rawValue {
		return TheTrue
	} else {
		return TheFalse
	}
}

type Integer struct {
	RawValue int
}

func (value *Integer) String() string {
	return fmt.Sprintf("%d", value.RawValue)
}

func (value *Integer) Boolean() bool {
	panic("not boolean")
}

type Symbol struct {
	Name string
}

func (value *Symbol) String() string {
	return value.Name
}

func (value *Symbol) Boolean() bool {
	panic("not boolean")
}

var symbolPool = make(map[string]*Symbol)

func MakeSymbol(name string) *Symbol {
	if symbol, ok := symbolPool[name]; ok {
		return symbol
	}
	symbol := &Symbol{name}
	symbolPool[name] = symbol
	return symbol
}

type FunctionValue interface {
	Value
	Apply(args []Value) Value
}
