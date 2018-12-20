package main

import (
	"fmt"
	"io/ioutil"
	"os"

	"minilisp/data"
	"minilisp/eval"
	"minilisp/parser"
)

func batchMain(filename string) error {
	codeBytes, err := ioutil.ReadFile(filename)
	if err != nil {
		return fmt.Errorf("failed to open the input file: %v", err)
	}
	code := string(codeBytes)

	exprs, err := parser.Parse(code)
	if err != nil {
		return fmt.Errorf("code failed to parse: %v", err)
	}
	env := data.NewTopLevelEnv()
	for _, expr := range exprs {
		_, err := eval.Evaluate(env, expr)
		if err != nil {
			return err
		}
	}
	return nil
}

func main() {
	if len(os.Args) == 1 {
		fmt.Fprintln(os.Stderr, "REPL not implemented")
	} else if len(os.Args) > 2 {
		fmt.Fprintln(os.Stderr, "usage: minilisp <file>")
	} else {
		if err := batchMain(os.Args[1]); err != nil {
			fmt.Fprintln(os.Stderr, "ERROR:", err)
		}
	}
}
