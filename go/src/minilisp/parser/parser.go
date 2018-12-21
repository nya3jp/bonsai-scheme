package parser

import (
	"errors"
	"regexp"
	"strconv"
	"strings"

	"minilisp/data"
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

func newQuote(value data.Value) data.Value {
	return data.SliceToValue([]data.Value{data.NewSymbol("quote"), value})
}

func parseValue(code string) (data.Value, string, error) {
	if strings.HasPrefix(code, "'") {
		value, restCode, err := parseValue(code[1:])
		if err != nil {
			return nil, "", err
		}
		return newQuote(value), restCode, nil
	}

	if strings.HasPrefix(code, "(") {
		values, restCode, err := parseList(code[1:])
		if err != nil {
			return nil, "", err
		}
		restCode = skip(restCode)
		if !strings.HasPrefix(restCode, ")") {
			return nil, "", errors.New("unexpected end of list")
		}
		return data.SliceToValue(values), restCode[1:], nil
	}

	loc := tokenRegexp.FindStringIndex(code)
	if loc == nil {
		return nil, "", errors.New("invalid token")
	}
	token := code[loc[0]:loc[1]]
	var value data.Value
	if numberRegexp.MatchString(token) {
		num, err := strconv.Atoi(token)
		if err != nil {
			return nil, "", err
		}
		value = data.NewInt(num)
	} else if token == "#f" {
		value = data.TheFalse
	} else if token == "#t" {
		value = data.TheTrue
	} else {
		value = data.NewSymbol(token)
	}
	return value, code[loc[1]:], nil
}

func parseList(code string) ([]data.Value, string, error) {
	var values []data.Value
	for {
		code = skip(code)
		if len(code) == 0 || strings.HasPrefix(code, ")") {
			break
		}
		value, newCode, err := parseValue(code)
		if err != nil {
			return nil, "", err
		}
		values = append(values, value)
		code = newCode
	}
	return values, code, nil
}

func ParseValue(code string) (data.Value, error) {
	value, excessCode, err := parseValue(code)
	if err != nil {
		return nil, err
	}
	if len(excessCode) > 0 {
		return nil, errors.New("excess code")
	}
	return value, nil
}

func ParseList(code string) ([]data.Value, error) {
	values, excessCode, err := parseList(code)
	if err != nil {
		return nil, err
	}
	if len(excessCode) > 0 {
		return nil, errors.New("excess parentheses")
	}
	return values, nil
}
