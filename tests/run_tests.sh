#!/bin/bash

cd "$(dirname "$0")"

set -ex

[[ -x ./compile.sh ]] && ./compile.sh

exec ../tests/run_tests.py ./minscheme
