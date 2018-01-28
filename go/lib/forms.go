package lib

func formQuote(env *Environment, rawArgs []Value) Value {
	if len(rawArgs) != 1 {
		panic("args")
	}
	return rawArgs[0]
}

func evaluateBody(env *Environment, exprs []Value) Value {
	var result Value = TheUndef
	for _, expr := range(exprs) {
		result = env.Evaluate(expr)
	}
	return result
}

func formBegin(env *Environment, rawArgs []Value) Value {
	return evaluateBody(env, rawArgs)
}

var formMap = make(map[string]func(*Environment, []Value) Value)

func init() {
	formMap["quote"] = formQuote
	formMap["begin"] = formBegin
}
