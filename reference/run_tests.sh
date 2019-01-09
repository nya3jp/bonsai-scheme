#!/bin/bash

cd "$(dirname "$0")"

exec ../tests/run_tests.py gosh
