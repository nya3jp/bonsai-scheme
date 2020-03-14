#!/bin/bash

cd "$(dirname "$0")"

set -ex

exec stack install --local-bin-path "$PWD"
