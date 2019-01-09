package data

import (
	"fmt"
)

type Variable struct {
	Value Value
}

type Env struct {
	parent *Env
	vars   map[string]*Variable
}

func NewEnv(parent *Env) *Env {
	return &Env{parent: parent, vars: make(map[string]*Variable)}
}

func NewTopLevelEnv() *Env {
	env := NewEnv(nil)
	for name, value := range builtins {
		env.Ensure(name).Value = value
	}
	return env
}

func (e *Env) Ensure(name string) *Variable {
	v, ok := e.vars[name]
	if !ok {
		v = &Variable{TheUndef}
		e.vars[name] = v
	}
	return v
}

func (e *Env) Lookup(name string) (*Variable, error) {
	v, ok := e.vars[name]
	if ok {
		return v, nil
	}
	if e.parent == nil {
		return nil, fmt.Errorf("name not found: %s", name)
	}
	return e.parent.Lookup(name)
}
