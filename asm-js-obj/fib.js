/* Some experiments with asm.js */

function fibmod(stdlib, ffi/*, heap*/) {
    "use asm";

    function fib(n) {
	n = +n;
	if (n < 2.0) return n;
	return +(+fib(n-1.0) + +fib(n-2.0));
    }

    return { fib: fib }
}

var exports = fibmod(this);
print(exports.fib(30));
