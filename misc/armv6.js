function m(stdlib, ffi, heap) {
    "use asm";

    var i8 = new stdlib.SharedInt8Array(heap);
    var i16 = new stdlib.SharedInt16Array(heap);
    var add = stdlib.Atomics.add;
    var cas = stdlib.Atomics.compareExchange;
    var ld = stdlib.Atomics.load;
    var st = stdlib.Atomics.store;

    function f() {
	add(i8, 5, 37);
    };

    function g() {
	add(i16, 8, 37);
    }

    function h() {
	cas(i8, 5, 79, 18);
    }

    function k() {
	cas(i16, 8, 79, 33);
    }

    function d1() {
	return ld(i8, 5);
    }

    function d2() {
	return ld(i16, 8);
    }

    function e1() {
	return st(i8, 5, 9);
    }

    function e2() {
	return st(i16, 8, 17);
    }

    return {f:f, g:g, h:h, k:k, d1:d1, d2:d2, e1:e1, e2:e2};
}

var sab = new SharedArrayBuffer(65536);
var i8 = new SharedInt8Array(sab);
var i16 = new SharedInt16Array(sab);
i8[5] = 42;
i16[8] = 42;
var {f,g,h,k,d1,d2,e1,e2} = m(this, {}, sab);

f();
assertEq(i8[5], 42+37);
g();
assertEq(i16[8], 42+37);
h();
assertEq(i8[5], 18);
k();
assertEq(i16[8], 33);
assertEq(d1(), 18);
assertEq(d2(), 33);
e1();
assertEq(i8[5], 9);
e2();
assertEq(i16[8], 17);
