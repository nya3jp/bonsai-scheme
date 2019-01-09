#!/bin/bash

cd "$(dirname "$0")"

set -ex

exec ../tests/run_tests.py ./run_main.sh
