package data

import "errors"

func ValueToSlice(value Value) ([]Value, error) {
	var slice []Value
	cur := value
	for {
		if _, ok := cur.(*Null); ok {
			break
		} else if pair, ok := cur.(*Pair); ok {
			slice = append(slice, pair.Car)
			cur = pair.Cdr
		} else {
			return nil, errors.New("not a list value")
		}
	}
	return slice, nil
}

func SliceToValue(slice []Value) Value {
	if len(slice) == 0 {
		return TheNull
	}
	return NewPair(slice[0], SliceToValue(slice[1:]))
}
