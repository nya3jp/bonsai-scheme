#!/bin/bash

export PYTHONPATH="$(dirname "$0")"

set -ex

exec python3 -m minilisp "$@"
