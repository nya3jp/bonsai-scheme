package data

import (
	"bytes"
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
	var buf bytes.Buffer
	fmt.Fprintf(&buf, "(%v", v.Car)
	cur := v.Cdr
	for cur != nil {
		switch v := cur.(type) {
		case *Null:
			fmt.Fprintf(&buf, ")")
			cur = nil
		case *Pair:
			fmt.Fprintf(&buf, " %v", v.Car)
			cur = v.Cdr
		default:
			fmt.Fprintf(&buf, " . %v)", v)
			cur = nil
		}
	}
	return buf.String()
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

type Func struct {
	name string
	f    func(args []Value) (Value, error)
}

func NewFunc(name string, f func(args []Value) (Value, error)) *Func {
	return &Func{name, f}
}

func (v *Func) String() string {
	return v.name
}

func (v *Func) Call(args []Value) (Value, error) {
	return v.f(args)
}
