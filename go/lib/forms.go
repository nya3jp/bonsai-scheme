package lib

func formQuote(env *Environment, rawArgs []Value) Value {
	if len(rawArgs) != 1 {
		panic("args")
	}
	return rawArgs[0]
}

func evaluateBody(env *Environment, exprs []Value) Value {
	var result Value = TheUndef
	for _, expr := range exprs {
		result = env.Evaluate(expr)
	}
	return result
}

func formBegin(env *Environment, rawArgs []Value) Value {
	return evaluateBody(env, rawArgs)
}

type LambdaFunction struct {
	parentEnv *Environment
	name      string
	params    []string
	body      []Value
}

func (f *LambdaFunction) String() string {
	return f.name
}

func (f *LambdaFunction) Apply(args []Value) Value {
	env := makeEnv(f.parentEnv)
	if len(f.params) != len(args) {
		panic("wrong number of args")
	}
	for i, param := range f.params {
		arg := args[i]
		env.Ensure(param).Value = arg
	}
	return evaluateBody(env, f.body)
}

func makeLambdaFunction(env *Environment, name string, paramsValue ListValue, body []Value) *LambdaFunction {
	params := []string{}
	for _, value := range paramsValue.ToSlice() {
		params = append(params, value.(*Symbol).Name)
	}
	return &LambdaFunction{env, name, params, body}
}

func formLambda(env *Environment, rawArgs []Value) Value {
	if len(rawArgs) == 0 {
		panic("args")
	}
	return makeLambdaFunction(env, "<lambda>", rawArgs[0].(ListValue), rawArgs[1:])
}

func formDefine(env *Environment, rawArgs []Value) Value {
	if len(rawArgs) == 0 {
		panic("args")
	}
	if sym, ok := rawArgs[0].(*Symbol); ok {
		if len(rawArgs) != 2 {
			panic("args")
		}
		value := env.Evaluate(rawArgs[1])
		env.Ensure(sym.Name).Value = value
		return TheUndef
	}
	target := rawArgs[0].(*Pair)
	name := target.Car.(*Symbol).Name
	paramsValue := target.Cdr.(ListValue)
	env.Ensure(name).Value = makeLambdaFunction(env, name, paramsValue, rawArgs[1:])
	return TheUndef
}

func formIf(env *Environment, rawArgs []Value) Value {
	if !(len(rawArgs) == 2 || len(rawArgs) == 3) {
		panic("args")
	}
	testValue := env.Evaluate(rawArgs[0])
	if testValue != TheFalse {
		return env.Evaluate(rawArgs[1])
	} else {
		if len(rawArgs) == 3 {
			return env.Evaluate(rawArgs[2])
		}
		return TheUndef
	}
}

func formCond(env *Environment, rawArgs []Value) Value {
	for _, clauseValue := range rawArgs {
		clause := clauseValue.(ListValue).ToSlice()
		if len(clause) != 2 {
			panic("args")
		}
		if sym, ok := clause[0].(*Symbol); ok && sym.Name == "else" {
			return env.Evaluate(clause[1])
		} else if env.Evaluate(clause[0]) != TheFalse {
			return env.Evaluate(clause[1])
		}
	}
	return TheUndef
}

func formLet(env *Environment, rawArgs []Value) Value {
	letEnv := makeEnv(env)
	for _, bindingValue := range rawArgs[0].(ListValue).ToSlice() {
		binding := bindingValue.(ListValue).ToSlice()
		if len(binding) != 2 {
			panic("args")
		}
		name := binding[0].(*Symbol).Name
		letEnv.Ensure(name).Value = env.Evaluate(binding[1])
	}
	return evaluateBody(letEnv, rawArgs[1:])
}

func formLetStar(env *Environment, rawArgs []Value) Value {
	letEnv := makeEnv(env)
	for _, bindingValue := range rawArgs[0].(ListValue).ToSlice() {
		binding := bindingValue.(ListValue).ToSlice()
		if len(binding) != 2 {
			panic("args")
		}
		name := binding[0].(*Symbol).Name
		letEnv.Ensure(name).Value = letEnv.Evaluate(binding[1])
	}
	return evaluateBody(letEnv, rawArgs[1:])
}

func formSet(env *Environment, rawArgs []Value) Value {
	if len(rawArgs) != 2 {
		panic("args")
	}
	name := rawArgs[0].(*Symbol).Name
	env.Lookup(name).Value = env.Evaluate(rawArgs[1])
	return TheUndef
}

var formMap = make(map[string]func(*Environment, []Value) Value)

func init() {
	formMap["quote"] = formQuote
	formMap["begin"] = formBegin
	formMap["lambda"] = formLambda
	formMap["define"] = formDefine
	formMap["if"] = formIf
	formMap["cond"] = formCond
	formMap["let"] = formLet
	formMap["let*"] = formLetStar
	formMap["set!"] = formSet
}
