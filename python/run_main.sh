#!/bin/bash

export PYTHONPATH="$(dirname "$0")"
exec python3 -m minilisp "$@"
