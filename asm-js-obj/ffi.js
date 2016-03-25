function testmod(stdlib, ffi/*, heap*/) {
    "use asm";

    const myfun = ffi.myfun;

    function f(x) {
	x = x|0;
	myfun(x|0);
    }

    return f;
}

testmod(this, { myfun: function (x) { print(x); } })(37);
