#!/bin/bash

cd "$(dirname "$0")"

set -ex

export GOPATH=$PWD

exec go get minilisp/cmd/minilisp
