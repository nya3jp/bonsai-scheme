package main

import (
	"bufio"
	"fmt"
	"io/ioutil"
	"os"

	"github.com/nya3jp/bonsai-scheme/go/data"
	"github.com/nya3jp/bonsai-scheme/go/eval"
	"github.com/nya3jp/bonsai-scheme/go/parser"
)

func batchMain(filename string) error {
	codeBytes, err := ioutil.ReadFile(filename)
	if err != nil {
		return fmt.Errorf("failed to open the input file: %v", err)
	}
	code := string(codeBytes)

	exprs, err := parser.ParseList(code)
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

func interactiveMain() {
	env := data.NewTopLevelEnv()
	sc := bufio.NewScanner(os.Stdin)
	for {
		fmt.Print("> ")
		if !sc.Scan() {
			break
		}
		code := sc.Text()
		expr, err := parser.ParseValue(code)
		if err != nil {
			fmt.Printf("ERROR: failed to parse: %v\n", err)
			continue
		}
		value, err := eval.Evaluate(env, expr)
		if err != nil {
			fmt.Printf("ERROR: failed to evaluate: %v\n", err)
			continue
		}
		if value != data.TheUndef {
			fmt.Println(value)
		}
	}
}

func main() {
	if len(os.Args) == 1 {
		interactiveMain()
	} else if len(os.Args) > 2 {
		fmt.Fprintln(os.Stderr, "usage: minscheme <file>")
	} else {
		if err := batchMain(os.Args[1]); err != nil {
			fmt.Fprintln(os.Stderr, "ERROR:", err)
		}
	}
}
