// Basic functional tests for the Atomics primitives.  Do not test atomicity, just that
// calling and coercions and indexing and exception behavior all work right.

function testMethod(a, ...indices) {
    print("Method: " + a.constructor.name);
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

print("Done");
