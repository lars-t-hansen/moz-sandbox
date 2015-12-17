#!/bin/bash

JS_SHELL=dist/bin/js
JS_OPT=
TEST_DIR=$(dirname $0)

function runtest() {
    echo $1
    $JS_SHELL $JS_OPT $TEST_DIR/$1
}

runtest globals.js
runtest arithmetic.js
runtest bitwise.js
runtest relations.js
runtest stmt.js
runtest misc.js
