#!/bin/bash

set -eux
set -o pipefail

COOKIE="cookie:session=$AOC_SESSION"
URL="https://adventofcode.com/2024/day/$1/input"
FILE="inputs/$1.txt"

curl --fail --silent --show-error -H $COOKIE $URL --output $FILE
