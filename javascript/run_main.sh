#!/bin/bash

readonly index="$(dirname "$0")/minscheme/index.js"

set -ex

exec node "$index" "$@"
