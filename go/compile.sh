#!/bin/bash

cd "$(dirname "$0")"

set -ex

exec go build github.com/nya3jp/minscheme/go/cmd/minscheme
