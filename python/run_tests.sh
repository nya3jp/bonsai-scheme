#!/bin/bash

cd "$(dirname "$0")"

exec ../tests/run_tests.py ./run_main.sh
