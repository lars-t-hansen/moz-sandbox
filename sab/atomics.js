if (!this.SharedArrayBuffer || !this.SharedInt32Array || !this.Atomics)
    quit();

function loadModule(stdlib, foreign, heap) {
    "use asm";

    var atomic_fence = stdlib.Atomics.fence;
    var atomic_load = stdlib.Atomics.load;
    var atomic_store = stdlib.Atomics.store;
    var atomic_cmpxchg = stdlib.Atomics.compareExchange;
    var atomic_add = stdlib.Atomics.add;
    var atomic_sub = stdlib.Atomics.sub;
    var atomic_and = stdlib.Atomics.and;
    var atomic_or = stdlib.Atomics.or;
    var atomic_xor = stdlib.Atomics.xor;

    var i32a = new stdlib.SharedInt32Array(heap);

    function do_fence() {
	atomic_fence();
    }

    // Load element 0
    function do_load() {
	var v = 0;
	v = atomic_load(i32a, 0)|0;
	return v|0;
    }

    // Add 37 to element 10
    function do_add() {
	var v = 0;
	v = atomic_add(i32a, 10, 37)|0;
	return v|0;
    }

    // Subtract 148 from element 20
    function do_sub() {
	var v = 0;
	v = atomic_sub(i32a, 20, 148)|0;
	return v|0;
    }

    // CAS element 100: 0 -> -1
    function do_cas1() {
	var v = 0;
	v = atomic_cmpxchg(i32a, 100, 0, -1)|0;
	return v|0;
    }

    // CAS element 100: -1 -> 0x5A5A5A5A
    function do_cas2() {
	var v = 0;
	v = atomic_cmpxchg(i32a, 100, -1, 0x5A5A5A5A)|0;
	return v|0;
    }

    return { fence: do_fence,
	     load: do_load,
	     add: do_add,
	     sub: do_sub,
	     cas1: do_cas1,
	     cas2: do_cas2 };
}

// TODO: byte arrays
// TODO: halfword arrays
// TODO: signed vs unsigned; negative results

var heap = new SharedArrayBuffer(65536);
var i32a = new SharedInt32Array(heap);
var module = loadModule(this, {}, heap);

module.fence();

i32a[0] = 12345;
assertEq(module.load(), 12345);

i32a[10] = 18;
assertEq(module.add(), 18);
assertEq(i32a[10], 18+37);

i32a[20] = 4972;
assertEq(module.sub(), 4972);
assertEq(i32a[20], 4972 - 148);

i32a[100] = 0;
assertEq(module.cas1(), 0);
assertEq(module.cas2(), -1);
assertEq(i32a[100], 0x5A5A5A5A);

print("Done");
