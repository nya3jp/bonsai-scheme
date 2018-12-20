#!/bin/bash

cd "$(dirname "$0")"

set -ex

./compile.sh
exec ../tests/run_tests.py ./bin/minilisp
