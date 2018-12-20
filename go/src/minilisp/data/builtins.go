package data

import (
	"errors"
	"fmt"
)

func print(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("print got %d arg(s); want 1", len(args))
	}
	fmt.Println(args[0])
	return TheUndef, nil
}

func parseInts(name string, args []Value) ([]int, error) {
	if len(args) == 0 {
		return nil, fmt.Errorf("%s got %d arg(s); want 1+", name, len(args))
	}
	var is []int
	for _, arg := range args {
		v, ok := arg.(*Int)
		if !ok {
			return nil, fmt.Errorf("%s failed: not an int", name)
		}
		is = append(is, v.I)
	}
	return is, nil
}

func add(args []Value) (Value, error) {
	is, err := parseInts("+", args)
	if err != nil {
		return nil, err
	}
	a := 0
	for _, i := range is {
		a += i
	}
	return NewInt(a), nil
}

func sub(args []Value) (Value, error) {
	is, err := parseInts("-", args)
	if err != nil {
		return nil, err
	}
	a := is[0]
	for _, i := range is[1:] {
		a -= i
	}
	return NewInt(a), nil
}

func mul(args []Value) (Value, error) {
	is, err := parseInts("*", args)
	if err != nil {
		return nil, err
	}
	a := 1
	for _, i := range is {
		a *= i
	}
	return NewInt(a), nil
}

func div(args []Value) (Value, error) {
	is, err := parseInts("/", args)
	if err != nil {
		return nil, err
	}
	a := is[0]
	for _, i := range is[1:] {
		a /= i
	}
	return NewInt(a), nil
}

func parseTwoInts(name string, args []Value) (int, int, error) {
	if len(args) != 2 {
		return 0, 0, fmt.Errorf("%s got %d arg(s); want 2", name, len(args))
	}
	lhs, ok := args[0].(*Int)
	if !ok {
		return 0, 0, fmt.Errorf("%s failed: not an int", name)
	}
	rhs, ok := args[1].(*Int)
	if !ok {
		return 0, 0, fmt.Errorf("%s failed: not an int", name)
	}
	return lhs.I, rhs.I, nil
}

func eq(args []Value) (Value, error) {
	lhs, rhs, err := parseTwoInts("=", args)
	if err != nil {
		return nil, err
	}
	return NewBool(lhs == rhs), nil
}

func lt(args []Value) (Value, error) {
	lhs, rhs, err := parseTwoInts("<", args)
	if err != nil {
		return nil, err
	}
	return NewBool(lhs < rhs), nil
}

func lte(args []Value) (Value, error) {
	lhs, rhs, err := parseTwoInts("<=", args)
	if err != nil {
		return nil, err
	}
	return NewBool(lhs <= rhs), nil
}

func gt(args []Value) (Value, error) {
	lhs, rhs, err := parseTwoInts(">", args)
	if err != nil {
		return nil, err
	}
	return NewBool(lhs > rhs), nil
}

func gte(args []Value) (Value, error) {
	lhs, rhs, err := parseTwoInts(">=", args)
	if err != nil {
		return nil, err
	}
	return NewBool(lhs >= rhs), nil
}

func and(args []Value) (Value, error) {
	result := true
	for _, arg := range args {
		result = result && arg != TheFalse
	}
	return NewBool(result), nil
}

func or(args []Value) (Value, error) {
	result := false
	for _, arg := range args {
		result = result || arg != TheFalse
	}
	return NewBool(result), nil
}

func not(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("not got %d arg(s); want 1", len(args))
	}
	return NewBool(!(args[0] != TheFalse)), nil
}

func eqCheck(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("eq? got %d arg(s); want 2", len(args))
	}
	return NewBool(args[0] == args[1]), nil
}

func cons(args []Value) (Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("cons got %d arg(s); want 2", len(args))
	}
	return NewPair(args[0], args[1]), nil
}

func car(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("car got %d arg(s); want 1", len(args))
	}
	pair, ok := args[0].(*Pair)
	if !ok {
		return nil, errors.New("car failed: not a pair")
	}
	return pair.Car, nil
}

func cdr(args []Value) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("cdr got %d arg(s); want 1", len(args))
	}
	pair, ok := args[0].(*Pair)
	if !ok {
		return nil, errors.New("cdr failed: not a pair")
	}
	return pair.Cdr, nil
}

type nativeFunc struct {
	name string
	fun  func(args []Value) (Value, error)
}

func (f *nativeFunc) String() string {
	return f.name
}

func (f *nativeFunc) Apply(args []Value) (Value, error) {
	return f.fun(args)
}

var builtins = map[string]FuncValue{
	"print": &nativeFunc{"print", print},
	"+":     &nativeFunc{"+", add},
	"-":     &nativeFunc{"-", sub},
	"*":     &nativeFunc{"*", mul},
	"/":     &nativeFunc{"/", div},
	"=":     &nativeFunc{"=", eq},
	"<":     &nativeFunc{"<", lt},
	"<=":    &nativeFunc{"<=", lte},
	">":     &nativeFunc{">", gt},
	">=":    &nativeFunc{">=", gte},
	"and":   &nativeFunc{"and", and},
	"or":    &nativeFunc{"or", or},
	"not":   &nativeFunc{"not", not},
	"eq?":   &nativeFunc{"eq?", eqCheck},
	"cons":  &nativeFunc{"cons", cons},
	"car":   &nativeFunc{"car", car},
	"cdr":   &nativeFunc{"cdr", cdr},
}
