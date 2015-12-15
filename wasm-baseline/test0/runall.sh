#!/bin/bash

JS_SHELL=dist/bin/js
TEST_DIR=$(dirname $0)

function runtest() {
    echo $1
    $JS_SHELL $TEST_DIR/$1
}

runtest globals.js
runtest arithmetic.js
runtest bitwise.js
