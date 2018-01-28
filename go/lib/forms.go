package lib

func formQuote(env *Environment, rawArgs []Value) Value {
	if len(rawArgs) != 1 {
		panic("quote")
	}
	return rawArgs[0]
}

var formMap = map[string]func(*Environment, []Value) Value {
	"quote": formQuote,
}
