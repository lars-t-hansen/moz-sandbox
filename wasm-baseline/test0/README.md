The test0 test cases are not wasm test cases per se, they are asm.js
test cases that trigger various behaviors in SpiderMonkey's
asm-to-wasm translator pipeline and subsequently in the wasm
compilers.  They therefore test the wasm "extended subset" generated
by the asm-to-wasm pipeline.

The script runall.sh runs all the tests in an appropriate order.

Missing tests (given feature set as of writing):

- double literals (just returning the value)

Working tests (given feature set as of writing):

- i32: global refs, sets
- f64: global refs, sets
- i32: literals
- i32: add, subtract
- f64: add, subtract
- i32: negate
- f64: negate
- i32: bitand, bitor, bitxor, bitnot
- i32: lshift, rshift, urshift
- i32: comparison operators
- i32: logical not
- conditional operator
- comma operator
- if
- blocks
- while, while-break, while-break-label, while-continue, while-continue-label
- do, do-break, do-break-label, do-continue, do-continue-label
- for, for-continue
- switch

