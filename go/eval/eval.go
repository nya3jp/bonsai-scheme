package eval

import (
	"errors"
	"fmt"

	"github.com/nya3jp/minilisp/go/data"
)

func Evaluate(env *data.Env, expr data.Value) (data.Value, error) {
	switch expr := expr.(type) {
	case *data.Undef, *data.Bool, *data.Int:
		return expr, nil
	case *data.Symbol:
		v, err := env.Lookup(expr.Name)
		if err != nil {
			return nil, err
		}
		return v.Value, nil
	case *data.Pair:
		rawArgs, err := data.ValueToSlice(expr.Cdr)
		if err != nil {
			return nil, fmt.Errorf("invalid function call: %v", err)
		}
		if sym, ok := expr.Car.(*data.Symbol); ok {
			if f, ok := formMap[sym.Name]; ok {
				return f(env, rawArgs)
			}
		}
		v, err := Evaluate(env, expr.Car)
		if err != nil {
			return nil, err
		}
		fun, ok := v.(data.FuncValue)
		if !ok {
			return nil, errors.New("can not call non-function value")
		}
		var args []data.Value
		for _, rawArg := range rawArgs {
			v, err := Evaluate(env, rawArg)
			if err != nil {
				return nil, err
			}
			args = append(args, v)
		}
		return fun.Apply(args)
	default:
		return nil, fmt.Errorf("not evaluatable: %s", expr)
	}
}
