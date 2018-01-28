# MiniLisp

MiniLisp is a collection of minimal LISP implementations in various
programming languages.

These implementations are toys and not intended for real use.

Note that one of the goal of MiniLisp implementations is to keep the code short,
thus they aggressively omit error handling.


## Language Spec

LISP language implemented here is a subset of Scheme. It differs from Scheme
in many aspects, but notable differences are:

- No macro.
- No variable-length arguments.
- Very limited special forms and builtin functions.

Supported features:

- Types: integer, boolean, nil, pair, symbol, function
- Special forms: quote, begin, lambda, define, if, cond, let, let*, set!
- Builtin functions: print, +, -, *, /, =, <, <=, >, >=, and, or, not, eq?, cons, car, cdr


## Tests

`tests` directory contains several basic test cases. They can be run by
`run_tests.sh` script in each implementation directories.
