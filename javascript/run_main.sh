#!/bin/bash

readonly index="$(dirname "$0")/minilisp/index.js"

set -ex

exec node "$index" "$@"
