package lib

import (
	"regexp"
	"strconv"
	"strings"
)

var (
	skipRegexp   = regexp.MustCompile(`^(\s+|;.*)+`)
	tokenRegexp  = regexp.MustCompile(`^[^\s);]+`)
	numberRegexp = regexp.MustCompile(`^-?[0-9]+$`)
)

func skip(code string) string {
	loc := skipRegexp.FindStringIndex(code)
	if loc == nil {
		return code
	}
	return code[loc[1]:]
}

func makeQuote(value Value) Value {
	return ToListValue([]Value{MakeSymbol("quote"), value})
}

func parseValue(code string) (Value, string) {
	if strings.HasPrefix(code, "'") {
		value, restCode := parseValue(code[1:])
		return makeQuote(value), restCode
	}

	if strings.HasPrefix(code, "(") {
		values, restCode := parseList(code[1:])
		restCode = skip(restCode)
		if !strings.HasPrefix(restCode, ")") {
			panic(restCode)
		}
		return ToListValue(values), restCode[1:]
	}

	loc := tokenRegexp.FindStringIndex(code)
	token := code[loc[0]:loc[1]]
	var value Value
	if numberRegexp.MatchString(token) {
		num, _ := strconv.Atoi(token)
		value = &Integer{num}
	} else if token == "#f" {
		value = TheFalse
	} else if token == "#t" {
		value = TheTrue
	} else {
		value = MakeSymbol(token)
	}
	return value, code[loc[1]:]
}

func parseList(code string) ([]Value, string) {
	values := make([]Value, 0)
	for {
		code = skip(code)
		if len(code) == 0 || strings.HasPrefix(code, ")") {
			break
		}
		var value Value
		value, code = parseValue(code)
		values = append(values, value)
	}
	return values, code
}

func Parse(code string) []Value {
	values, excessCode := parseList(code)
	if len(excessCode) > 0 {
		panic(excessCode)
	}
	return values
}
