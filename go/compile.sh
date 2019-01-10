#!/bin/bash

cd "$(dirname "$0")"

set -ex

exec go build github.com/nya3jp/bonsai-scheme/go/cmd/minscheme
