# Bonsai Scheme

[![Build Status](https://github.com/nya3jp/bonsai-scheme/actions/workflows/test.yml/badge.svg)](https://github.com/nya3jp/bonsai-scheme/actions/workflows/test.yml)

Bonsai Scheme is a collection of minimal Scheme implementations in various
programming languages.

These implementations are toys and not intended for real use.


## Language Spec

The language implemented here is a subset of Scheme. It differs from Scheme
in many aspects, but notable differences are:

- No garbage collection (unless it is easy to support).
- No macro.
- No variable-length arguments.
- Very limited special forms and builtin functions.
- Sparse error handling.

Supported features:

- Types: integer, boolean, nil, pair, symbol, function
- Special forms: quote, begin, lambda, define, if, cond, let, let\*, letrec, set!, set-car!, set-cdr!
- Builtin functions: print, +, -, \*, /, =, <, <=, >, >=, and, or, not, eq?, cons, car, cdr


## Tests

`tests` directory contains several basic test cases. They can be run by
`run_tests.sh` script in each implementation directories.
