package lib

import "fmt"

func builtinPrint(args []Value) Value {
	if len(args) != 1 {
		panic("print")
	}
	fmt.Println(args[0])
	return TheUndef
}

type nativeFunction struct {
	name string
	fun func(args []Value) Value
}

func (fun *nativeFunction) Boolean() bool {
	panic("not boolean")
}

func (fun *nativeFunction) String() string {
	return fun.name
}

func (fun *nativeFunction) Apply(args []Value) Value {
	return fun.fun(args)
}

var builtinMap = map[string]FunctionValue {
	"print": &nativeFunction{"print", builtinPrint},
}
