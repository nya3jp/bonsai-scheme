package lib

import "fmt"

func builtinPrint(args []Value) Value {
	if len(args) != 1 {
		panic("print")
	}
	fmt.Println(args[0])
	return TheUndef
}

func builtinAdd(args []Value) Value {
	if len(args) == 0 {
		panic("no args")
	}
	var result int = 0
	for _, arg := range args {
		result += arg.(*Integer).RawValue
	}
	return &Integer{result}
}

func builtinSub(args []Value) Value {
	if len(args) == 0 {
		panic("no args")
	}
	var result int = args[0].(*Integer).RawValue
	for i, arg := range args {
		if i == 0 {
			continue
		}
		result -= arg.(*Integer).RawValue
	}
	return &Integer{result}
}

func builtinMul(args []Value) Value {
	if len(args) == 0 {
		panic("no args")
	}
	var result int = 1
	for _, arg := range args {
		result *= arg.(*Integer).RawValue
	}
	return &Integer{result}
}

func builtinDiv(args []Value) Value {
	if len(args) == 0 {
		panic("no args")
	}
	var result int = args[0].(*Integer).RawValue
	for i, arg := range args {
		if i == 0 {
			continue
		}
		result /= arg.(*Integer).RawValue
	}
	return &Integer{result}
}

func builtinEq(args []Value) Value {
	if len(args) != 2 {
		panic("args")
	}
	lhs := args[0].(*Integer).RawValue
	rhs := args[1].(*Integer).RawValue
	return MakeBoolean(lhs == rhs)
}

func builtinLt(args []Value) Value {
	if len(args) != 2 {
		panic("args")
	}
	lhs := args[0].(*Integer).RawValue
	rhs := args[1].(*Integer).RawValue
	return MakeBoolean(lhs < rhs)
}

func builtinLte(args []Value) Value {
	if len(args) != 2 {
		panic("args")
	}
	lhs := args[0].(*Integer).RawValue
	rhs := args[1].(*Integer).RawValue
	return MakeBoolean(lhs <= rhs)
}

func builtinGt(args []Value) Value {
	if len(args) != 2 {
		panic("args")
	}
	lhs := args[0].(*Integer).RawValue
	rhs := args[1].(*Integer).RawValue
	return MakeBoolean(lhs > rhs)
}

func builtinGte(args []Value) Value {
	if len(args) != 2 {
		panic("args")
	}
	lhs := args[0].(*Integer).RawValue
	rhs := args[1].(*Integer).RawValue
	return MakeBoolean(lhs >= rhs)
}

func builtinAnd(args []Value) Value {
	var result bool = true
	for _, arg := range args {
		result = result && arg.Boolean()
	}
	return MakeBoolean(result)
}

func builtinOr(args []Value) Value {
	var result bool = false
	for _, arg := range args {
		result = result || arg.Boolean()
	}
	return MakeBoolean(result)
}

func builtinNot(args []Value) Value {
	if len(args) != 1 {
		panic("args")
	}
	return MakeBoolean(!args[0].(*Boolean).RawValue)
}

func builtinEqCheck(args []Value) Value {
	if len(args) != 2 {
		panic("args")
	}
	return MakeBoolean(args[0] == args[1])
}

func builtinEqualCheck(args []Value) Value {
	if len(args) != 2 {
		panic("args")
	}
	return MakeBoolean(args[0].DeepEquals(args[1]))
}

func builtinNullCheck(args []Value) Value {
	if len(args) != 1 {
		panic("args")
	}
	_, ok := args[0].(*Null)
	return MakeBoolean(ok)
}

func builtinPairCheck(args []Value) Value {
	if len(args) != 1 {
		panic("args")
	}
	_, ok := args[0].(*Pair)
	return MakeBoolean(ok)
}

func builtinBooleanCheck(args []Value) Value {
	if len(args) != 1 {
		panic("args")
	}
	_, ok := args[0].(*Boolean)
	return MakeBoolean(ok)
}

func builtinIntegerCheck(args []Value) Value {
	if len(args) != 1 {
		panic("args")
	}
	_, ok := args[0].(*Integer)
	return MakeBoolean(ok)
}

func builtinSymbolCheck(args []Value) Value {
	if len(args) != 1 {
		panic("args")
	}
	_, ok := args[0].(*Symbol)
	return MakeBoolean(ok)
}

func builtinCons(args []Value) Value {
	if len(args) != 2 {
		panic("args")
	}
	return &Pair{args[0], args[1]}
}

func builtinCar(args []Value) Value {
	if len(args) != 1 {
		panic("args")
	}
	return args[0].(*Pair).Car
}

func builtinCdr(args []Value) Value {
	if len(args) != 1 {
		panic("args")
	}
	return args[0].(*Pair).Cdr
}

type nativeFunction struct {
	name string
	fun  func(args []Value) Value
}

func (fun *nativeFunction) Boolean() bool {
	panic("not boolean")
}

func (fun *nativeFunction) String() string {
	return fun.name
}

func (fun *nativeFunction) DeepEquals(other Value) bool {
	return fun == other
}

func (fun *nativeFunction) Apply(args []Value) Value {
	return fun.fun(args)
}

var builtinMap = map[string]FunctionValue{
	"print":    &nativeFunction{"print", builtinPrint},
	"+":        &nativeFunction{"+", builtinAdd},
	"-":        &nativeFunction{"-", builtinSub},
	"*":        &nativeFunction{"*", builtinMul},
	"/":        &nativeFunction{"/", builtinDiv},
	"=":        &nativeFunction{"=", builtinEq},
	"<":        &nativeFunction{"<", builtinLt},
	"<=":       &nativeFunction{"<=", builtinLte},
	">":        &nativeFunction{">", builtinGt},
	">=":       &nativeFunction{">=", builtinGte},
	"and":      &nativeFunction{"and", builtinAnd},
	"or":       &nativeFunction{"or", builtinOr},
	"not":      &nativeFunction{"not", builtinNot},
	"eq?":      &nativeFunction{"eq?", builtinEqCheck},
	"equal?":   &nativeFunction{"equal?", builtinEqualCheck},
	"null?":    &nativeFunction{"null?", builtinNullCheck},
	"pair?":    &nativeFunction{"pair?", builtinPairCheck},
	"boolean?": &nativeFunction{"boolean?", builtinBooleanCheck},
	"integer?": &nativeFunction{"integer?", builtinIntegerCheck},
	"symbol?":  &nativeFunction{"symbol?", builtinSymbolCheck},
	"cons":     &nativeFunction{"cons", builtinCons},
	"car":      &nativeFunction{"car", builtinCar},
	"cdr":      &nativeFunction{"cdr", builtinCdr},
}
