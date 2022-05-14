#!/usr/bin/env bash

set -euo pipefail

if [ ! -d "$WD/bin" ]; then
    mkdir "$WD/bin"
fi

flags=(
    -D_GNU_SOURCE
    "-ferror-limit=1"
    "-fsanitize=address"
    "-fsanitize=bounds"
    "-fsanitize=float-divide-by-zero"
    "-fsanitize=implicit-conversion"
    "-fsanitize=integer"
    "-fsanitize=nullability"
    "-fsanitize=undefined"
    -fshort-enums
    "-march=native"
    -O1
    "-std=c99"
    -Werror
    -Weverything
    -Wno-covered-switch-default
    -Wno-declaration-after-statement
    -Wno-extra-semi-stmt
    -Wno-padded
    -Wno-reserved-macro-identifier
)

clang-format -i -verbose "$WD/src/"*
mold -run clang "${flags[@]}" -o "$WD/bin/main" "$WD/src/main.c"
