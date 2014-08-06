// Basic functional tests for the Atomics primitives.  Do not test atomicity, just that
// calling and coercions and indexing and exception behavior all work right.

function testMethod(a, ...indices) {
    print("Function: " + a.constructor.name);
    for ( var i=0 ; i < indices.length ; i++ ) {
	var x = indices[i];
	// val = 0
	assertEq(Atomics.compareExchange(a, x, 0, 37), 0);
	// val = 37
	assertEq(Atomics.compareExchange(a, x, 37, 5), 37);
	// val = 5
	assertEq(Atomics.compareExchange(a, x, 7, 8), 5); // ie should fail
	// val = 5
	assertEq(Atomics.compareExchange(a, x, 5, 9), 5);
	// val = 9
	assertEq(Atomics.compareExchange(a, x, 5, 0), 9); // should also fail

	// val = 9
	assertEq(Atomics.load(a, x), 9);
	// val = 9
	assertEq(Atomics.store(a, x, 14), 14); // What about coercion?
	// val = 14
	assertEq(Atomics.load(a, x), 14);
	// val = 14
	Atomics.store(a, x, 0);
	// val = 0

	Atomics.fence();

	// val = 0
	assertEq(Atomics.add(a, x, 3), 0);
	// val = 3
	assertEq(Atomics.sub(a, x, 2), 3);
	// val = 1
	assertEq(Atomics.or(a, x, 6), 1);
	// val = 7
	assertEq(Atomics.and(a, x, 14), 7);
	// val = 6
	assertEq(Atomics.xor(a, x, 5), 6);
	// val = 3
	assertEq(Atomics.load(a, x), 3);
	// val = 3
	Atomics.store(a, x, 0);
	// val = 0
    }
}

function testFunction(a, ...indices) {
    print("Function: " + a.constructor.name);
    for ( var i=0 ; i < indices.length ; i++ ) {
	var x = indices[i];
	// val = 0
	assertEq(gAtomics_compareExchange(a, x, 0, 37), 0);
	// val = 37
	assertEq(gAtomics_compareExchange(a, x, 37, 5), 37);
	// val = 5
	assertEq(gAtomics_compareExchange(a, x, 7, 8), 5); // ie should fail
	// val = 5
	assertEq(gAtomics_compareExchange(a, x, 5, 9), 5);
	// val = 9
	assertEq(gAtomics_compareExchange(a, x, 5, 0), 9); // should also fail

	// val = 9
	assertEq(gAtomics_load(a, x), 9);
	// val = 9
	assertEq(gAtomics_store(a, x, 14), 14); // What about coercion?
	// val = 14
	assertEq(gAtomics_load(a, x), 14);
	// val = 14
	gAtomics_store(a, x, 0);
	// val = 0

	gAtomics_fence();

	// val = 0
	assertEq(gAtomics_add(a, x, 3), 0);
	// val = 3
	assertEq(gAtomics_sub(a, x, 2), 3);
	// val = 1
	assertEq(gAtomics_or(a, x, 6), 1);
	// val = 7
	assertEq(gAtomics_and(a, x, 14), 7);
	// val = 6
	assertEq(gAtomics_xor(a, x, 5), 6);
	// val = 3
	assertEq(gAtomics_load(a, x), 3);
	// val = 3
	gAtomics_store(a, x, 0);
	// val = 0
    }
}

function testTypeCAS(a) {
    print("Type: " + a.constructor.name);

    var thrown = false;
    try {
	Atomics.compareExchange([0], 0, 0, 1);
    }
    catch (e) {
	thrown = true;
	assertEq(e instanceof TypeError, true);
    }
    assertEq(thrown, true);

    Atomics.compareExchange(a, 0, 0.7, 1.8); // doubles are allowed, this should not throw

    var thrown = false;
    try {
	Atomics.compareExchange(a, 0, "0", 1);
    }
    catch (e) {
	thrown = true;
	assertEq(e instanceof TypeError, true);
    }
    assertEq(thrown, true);

    var thrown = false;
    try {
	Atomics.compareExchange(a, 0, 0, "1");
    }
    catch (e) {
	thrown = true;
	assertEq(e instanceof TypeError, true);
    }
    assertEq(thrown, true);

    var thrown = false;
    try {
	Atomics.compareExchange(a, 0, 0); // Too few
    }
    catch (e) {
	thrown = true;
	assertEq(e instanceof TypeError, true);
    }
    assertEq(thrown, true);
}

function testTypeBinop(a, op) {
    print("Type: " + a.constructor.name);

    var thrown = false;
    try {
	op([0], 0, 1);
    }
    catch (e) {
	thrown = true;
	assertEq(e instanceof TypeError, true);
    }
    assertEq(thrown, true);

    op(a, 0, 0.7); // doubles are allowed, this should not throw

    var thrown = false;
    try {
	op(a, 0, "0");
    }
    catch (e) {
	thrown = true;
	assertEq(e instanceof TypeError, true);
    }
    assertEq(thrown, true);

    var thrown = false;
    try {
	op(a, 0); // Too few
    }
    catch (e) {
	thrown = true;
	assertEq(e instanceof TypeError, true);
    }
    assertEq(thrown, true);
}

function testRangeCAS(a) {
    print("Range: " + a.constructor.name);

    var thrown = false;
    try {
	Atomics.compareExchange(a, -1, 0, 1);
    }
    catch (e) {
	thrown = true;
	assertEq(e instanceof RangeError, true);
    }
    assertEq(thrown, true);

    var thrown = false;
    try {
	Atomics.compareExchange(a, a.length, 0, 1);
    }
    catch (e) {
	thrown = true;
	assertEq(e instanceof RangeError, true);
    }
    assertEq(thrown, true);

    var thrown = false;
    try {
	Atomics.compareExchange(a, "hi", 0, 1);
    }
    catch (e) {
	thrown = true;
	assertEq(e instanceof RangeError, true);
    }
    assertEq(thrown, true);
}

var sab = new SharedArrayBuffer(4096);

// Test that invoking as Atomics.whatever() works, on correct arguments

testMethod(new Int8Array(sab), 0, 42, 4095);
testMethod(new Uint8Array(sab), 0, 42, 4095);
testMethod(new Uint8ClampedArray(sab), 0, 42, 4095);
testMethod(new Int16Array(sab), 0, 42, 2047);
testMethod(new Uint16Array(sab), 0, 42, 2047);
testMethod(new Int32Array(sab), 0, 42, 1023);
testMethod(new Uint32Array(sab), 0, 42, 1023);

// Test that invoking as v = Atomics.whatever; v() works, on correct arguments

var gAtomics_compareExchange = Atomics.compareExchange;
var gAtomics_load = Atomics.load;
var gAtomics_store = Atomics.store;
var gAtomics_fence = Atomics.fence;
var gAtomics_add = Atomics.add;
var gAtomics_sub = Atomics.sub;
var gAtomics_and = Atomics.and;
var gAtomics_or = Atomics.or;
var gAtomics_xor = Atomics.xor;

testFunction(new Int8Array(sab), 0, 42, 4095);
testFunction(new Uint8Array(sab), 0, 42, 4095);
testFunction(new Uint8ClampedArray(sab), 0, 42, 4095);
testFunction(new Int16Array(sab), 0, 42, 2047);
testFunction(new Uint16Array(sab), 0, 42, 2047);
testFunction(new Int32Array(sab), 0, 42, 1023);
testFunction(new Uint32Array(sab), 0, 42, 1023);

// Test various range and type conditions

var v8 = new Int8Array(sab);
var v32 = new Int32Array(sab);

testRangeCAS(v8);
testRangeCAS(v32);

testTypeCAS(v8);
testTypeCAS(v32);

testTypeBinop(v8, Atomics.add);
testTypeBinop(v8, Atomics.sub);
testTypeBinop(v8, Atomics.and);
testTypeBinop(v8, Atomics.or);
testTypeBinop(v8, Atomics.xor);

testTypeBinop(v32, Atomics.add);
testTypeBinop(v32, Atomics.sub);
testTypeBinop(v32, Atomics.and);
testTypeBinop(v32, Atomics.or);
testTypeBinop(v32, Atomics.xor);

print("Done");
