#!/bin/bash

export PYTHONPATH="$(dirname "$0")"

set -ex

exec stack exec minilisp -- "$@"
