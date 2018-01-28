#!/bin/bash

cd "$(dirname "$0")"

set -ex

exec go build ./cmd/minilisp
