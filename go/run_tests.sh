#!/bin/bash

cd "$(dirname "$0")"

go build ./cmd/minilisp
exec ../tests/run_tests.py ./minilisp
