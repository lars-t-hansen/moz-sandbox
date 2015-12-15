The test0 test cases are not wasm test cases per se, they are asm.js
test cases that trigger various behaviors in SpiderMonkey's
asm-to-wasm translator pipeline and subsequently in the wasm
compilers.  They therefore test the wasm "extended subset" generated
by the asm-to-wasm pipeline.

The script runall.sh runs all the tests in an appropriate order.
