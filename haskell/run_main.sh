#!/bin/bash

set -ex

exec stack exec minilisp -- "$@"
