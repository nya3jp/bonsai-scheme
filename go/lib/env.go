package lib

import "fmt"

type Variable struct {
	Value Value
}

type Environment struct {
	Parent *Environment
	Vars map[string]*Variable
}

func (env *Environment) Ensure(name string) *Variable {
	v, ok := env.Vars[name]
	if !ok {
		v = &Variable{TheUndef}
		env.Vars[name] = v
	}
	return v
}

func (env *Environment) Lookup(name string) *Variable {
	v, ok := env.Vars[name]
	if ok {
		return v
	}
	if env.Parent == nil {
		panic("Name not found")
	}
	return env.Parent.Lookup(name)
}

func (env *Environment) Evaluate(expr Value) Value {
	if e, ok := expr.(*Undef); ok {
		return e
	}
	if e, ok := expr.(*Boolean); ok {
		return e
	}
	if e, ok := expr.(*Integer); ok {
		return e
	}
	if sym, ok := expr.(*Symbol); ok {
		return env.Lookup(sym.Name).Value
	}
	if pair, ok := expr.(*Pair); ok {
		rawArgs := pair.Cdr.(ListValue).ToSlice()
		if sym, ok := pair.Car.(*Symbol); ok {
			if f, ok := formMap[sym.Name]; ok {
				return f(env, rawArgs)
			}
		}
		fun := env.Evaluate(pair.Car).(FunctionValue)
		args := []Value{}
		for _, rawArg := range(rawArgs) {
			args = append(args, env.Evaluate(rawArg))
		}
		return fun.Apply(args)
	}
	panic(fmt.Sprintf("Can not evaluate: %s", expr))
}

func makeEnv(parent *Environment) *Environment {
	return &Environment{Parent: parent, Vars: make(map[string]*Variable)}
}

func MakeTopLevelEnv() *Environment {
	env := makeEnv(nil)
	for name, value := range(builtinMap) {
		env.Ensure(name).Value = value
	}
	return env
}
