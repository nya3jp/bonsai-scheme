package main

import (
	"io/ioutil"
	"fmt"
	"github.com/nya3jp/minilisp/go/lib"
	"os"
)

func batchMain(filename string) {
	codeBytes, err := ioutil.ReadFile(filename)
	if err != nil {
		fmt.Println("Failed to open the input file")
		return
	}
	code := string(codeBytes)

	exprs := lib.Parse(code)
	env := lib.MakeTopLevelEnv()
	for _, expr := range(exprs) {
		env.Evaluate(expr)
	}
}

func main() {
	if len(os.Args) == 1 {
		panic("REPL not implemented")
	} else if len(os.Args) == 2 {
		batchMain(os.Args[1])
	} else {
		panic("usage: minilisp [file]")
	}
}
