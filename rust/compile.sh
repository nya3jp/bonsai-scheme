#!/bin/bash

cd "$(dirname "$0")"

set -ex

exec cargo build
