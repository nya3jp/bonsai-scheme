package eval

import (
	"errors"
	"fmt"

	"github.com/nya3jp/minilisp/go/data"
)

func quote(env *data.Env, rawArgs []data.Value) (data.Value, error) {
	if len(rawArgs) != 1 {
		return nil, fmt.Errorf("quote got %d arg(s); want 1", len(rawArgs))
	}
	return rawArgs[0], nil
}

func evalBody(env *data.Env, exprs []data.Value) (data.Value, error) {
	var result data.Value = data.TheUndef
	for _, expr := range exprs {
		var err error
		result, err = Evaluate(env, expr)
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

func begin(env *data.Env, rawArgs []data.Value) (data.Value, error) {
	return evalBody(env, rawArgs)
}

type lambdaFunc struct {
	parentEnv *data.Env
	name      string
	params    []string
	body      []data.Value
}

func (f *lambdaFunc) String() string {
	return f.name
}

func (f *lambdaFunc) Apply(args []data.Value) (data.Value, error) {
	env := data.NewEnv(f.parentEnv)
	if len(args) != len(f.params) {
		return nil, fmt.Errorf("function got %d arg(s); want %d", len(args), len(f.params))
	}
	for i, param := range f.params {
		arg := args[i]
		env.Ensure(param).Value = arg
	}
	return evalBody(env, f.body)
}

func newLambdaFunc(env *data.Env, name string, rawParamsValue data.Value, body []data.Value) (*lambdaFunc, error) {
	var params []string
	rawParams, err := data.ValueToSlice(rawParamsValue)
	if err != nil {
		return nil, fmt.Errorf("malformed parameter list: %v", err)
	}
	for _, rawParam := range rawParams {
		sym, ok := rawParam.(*data.Symbol)
		if !ok {
			return nil, errors.New("non-symbol parameter")
		}
		params = append(params, sym.Name)
	}
	return &lambdaFunc{env, name, params, body}, nil
}

func lambda(env *data.Env, rawArgs []data.Value) (data.Value, error) {
	if len(rawArgs) == 0 {
		return nil, errors.New("lambda got 0 arg; want 1+")
	}
	value, err := newLambdaFunc(env, "<lambda>", rawArgs[0], rawArgs[1:])
	if err != nil {
		return nil, fmt.Errorf("malformed lambda: %v", err)
	}
	return value, nil
}

func define(env *data.Env, rawArgs []data.Value) (data.Value, error) {
	if len(rawArgs) == 0 {
		return nil, errors.New("define got 0 arg; want 1+")
	}
	if sym, ok := rawArgs[0].(*data.Symbol); ok {
		if len(rawArgs) != 2 {
			return nil, fmt.Errorf("define got %d arg(s); want 2", len(rawArgs))
		}
		value, err := Evaluate(env, rawArgs[1])
		if err != nil {
			return nil, err
		}
		env.Ensure(sym.Name).Value = value
		return data.TheUndef, nil
	}
	target, ok := rawArgs[0].(*data.Pair)
	if !ok {
		return nil, errors.New("malformed define: symbol or list required")
	}
	sym, ok := target.Car.(*data.Symbol)
	if !ok {
		return nil, errors.New("malformed define: non-symbol name")
	}
	name := sym.Name
	value, err := newLambdaFunc(env, name, target.Cdr, rawArgs[1:])
	if err != nil {
		return nil, fmt.Errorf("malformed define: %v", err)
	}
	env.Ensure(name).Value = value
	return data.TheUndef, nil
}

func ifs(env *data.Env, rawArgs []data.Value) (data.Value, error) {
	if !(len(rawArgs) == 2 || len(rawArgs) == 3) {
		return nil, fmt.Errorf("if got %d arg(s); want 2 or 3", len(rawArgs))
	}
	testValue, err := Evaluate(env, rawArgs[0])
	if err != nil {
		return nil, err
	}
	if testValue != data.TheFalse {
		return Evaluate(env, rawArgs[1])
	}
	if len(rawArgs) == 3 {
		return Evaluate(env, rawArgs[2])
	}
	return data.TheUndef, nil
}

func cond(env *data.Env, rawArgs []data.Value) (data.Value, error) {
	for _, clauseValue := range rawArgs {
		clause, err := data.ValueToSlice(clauseValue)
		if err != nil {
			return nil, fmt.Errorf("malformed cond: %v", err)
		}
		if len(clause) != 2 {
			return nil, errors.New("malformed cond: 2-size lists expected")
		}
		if sym, ok := clause[0].(*data.Symbol); ok && sym.Name == "else" {
			return Evaluate(env, clause[1])
		}
		value, err := Evaluate(env, clause[0])
		if err != nil {
			return nil, err
		}
		if value != data.TheFalse {
			return Evaluate(env, clause[1])
		}
	}
	return data.TheUndef, nil
}

type letEnvFunc func(curEnv, origEnv *data.Env) (nextEnv, evalEnv *data.Env)

func letCommon(name string, env *data.Env, rawArgs []data.Value, letEnvFunc letEnvFunc) (data.Value, error) {
	bindingValues, err := data.ValueToSlice(rawArgs[0])
	if err != nil {
		return nil, fmt.Errorf("malformed %s: 2-size lists expected", name)
	}
	curEnv := data.NewEnv(env)
	for _, bindingValue := range bindingValues {
		binding, err := data.ValueToSlice(bindingValue)
		if err != nil {
			return nil, fmt.Errorf("malformed %s: %v", name, err)
		}
		if len(binding) != 2 {
			return nil, fmt.Errorf("malformed %s: 2-size lists expected", name)
		}
		sym, ok := binding[0].(*data.Symbol)
		if !ok {
			return nil, fmt.Errorf("malformed %s: symbol expected", name)
		}
		nextEnv, evalEnv := letEnvFunc(curEnv, env)
		value, err := Evaluate(evalEnv, binding[1])
		if err != nil {
			return nil, err
		}
		curEnv = nextEnv
		curEnv.Ensure(sym.Name).Value = value
	}
	return evalBody(curEnv, rawArgs[1:])
}

func let(env *data.Env, rawArgs []data.Value) (data.Value, error) {
	return letCommon("let", env, rawArgs, func(curEnv, origEnv *data.Env) (nextEnv, evalEnv *data.Env) {
		return curEnv, origEnv
	})
}

func letStar(env *data.Env, rawArgs []data.Value) (data.Value, error) {
	return letCommon("let*", env, rawArgs, func(curEnv, origEnv *data.Env) (nextEnv, evalEnv *data.Env) {
		return data.NewEnv(curEnv), curEnv
	})
}

func letrec(env *data.Env, rawArgs []data.Value) (data.Value, error) {
	return letCommon("letrec", env, rawArgs, func(curEnv, origEnv *data.Env) (nextEnv, evalEnv *data.Env) {
		return curEnv, curEnv
	})
}

func set(env *data.Env, rawArgs []data.Value) (data.Value, error) {
	if len(rawArgs) != 2 {
		return nil, fmt.Errorf("set! got %d arg(s); want 2", len(rawArgs))
	}
	sym, ok := rawArgs[0].(*data.Symbol)
	if !ok {
		return nil, errors.New("malformed set!: not a symbol")
	}
	value, err := Evaluate(env, rawArgs[1])
	if err != nil {
		return nil, err
	}
	v, err := env.Lookup(sym.Name)
	if err != nil {
		return nil, err
	}
	v.Value = value
	return data.TheUndef, nil
}

var formMap map[string]func(*data.Env, []data.Value) (data.Value, error)

func init() {
	formMap = map[string]func(*data.Env, []data.Value) (data.Value, error){
		"quote":  quote,
		"begin":  begin,
		"lambda": lambda,
		"define": define,
		"if":     ifs,
		"cond":   cond,
		"let":    let,
		"let*":   letStar,
		"letrec": letrec,
		"set!":   set,
	}
}
