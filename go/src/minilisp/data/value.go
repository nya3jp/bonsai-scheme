package data

import (
	"fmt"
	"strconv"
)

type Value interface {
	fmt.Stringer
}

type Undef struct{}

var TheUndef *Undef

func (v *Undef) String() string {
	return "#undef"
}

type Null struct{}

var TheNull *Null

func (null *Null) String() string {
	return "()"
}

type Pair struct {
	Car Value
	Cdr Value
}

func NewPair(car, cdr Value) *Pair {
	return &Pair{car, cdr}
}

func (v *Pair) String() string {
	return fmt.Sprintf("(%v . %v)", v.Car, v.Cdr)
}

type Bool struct {
	RawValue bool
}

var (
	TheFalse = &Bool{false}
	TheTrue  = &Bool{true}
)

func NewBool(rawValue bool) *Bool {
	if rawValue {
		return TheTrue
	}
	return TheFalse
}

func (v *Bool) String() string {
	if v.RawValue {
		return "#t"
	}
	return "#f"
}

type Int struct {
	I int
}

func NewInt(value int) *Int {
	return &Int{value}
}

func (v *Int) String() string {
	return strconv.Itoa(v.I)
}

type Symbol struct {
	Name string
}

var symbolPool = make(map[string]*Symbol)

func NewSymbol(name string) *Symbol {
	if s, ok := symbolPool[name]; ok {
		return s
	}
	s := &Symbol{name}
	symbolPool[name] = s
	return s
}

func (v *Symbol) String() string {
	return v.Name
}

type FuncValue interface {
	Value
	Apply(args []Value) (Value, error)
}
