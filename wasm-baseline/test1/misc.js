// WASM baseline smoke tests.  "Baseline" matters a bit because we can
// count on few optimizations, so the tests can be simple, using
// constants and/or simple locals in many cases.

// Some of these test cases are run as asm.js.  Be sure to test with
// -w, and look for errors.  To debug, set VERBOSE to true below and
// run with --no-threads.

// TODO:

// Implemented but tested elsewhere:
//  control flow
//  calls
//  i32: load8s load8u load16s load16u load store8 store16 store
//  f32: load store
//  f64: load store

// asmjs: getglobal setglobal

// i32 not yet implemented: mul divs divu rems remu eqz truncsf32 truncuf32 truncsf64 truncuf64 wrapi64 reinterpretf32

// f32 not yet implemented: mul div neg abs sqrt demotef64 convertsi32 convertui32 convertsi64 convertui64 reinterpreti32 storef64

// f64 not yet implemented: mul div mod neg abs sqrt promotef32 convertsi32 convertui32 convertsi64 convertui64 storef32 reinterpreti64

// not yet implemented: select

// i64 not yet implemented: all of them
// atomics not yet implemented: all of them
// simd not yet implemented: all of them

var VERBOSE = false;

var libdir = "../../../mozilla-inbound/js/src/jit-test/lib/"

load(libdir + "wasm.js");

var _wasmEvalText = wasmEvalText;

wasmEvalText = function () {
    if (VERBOSE)
	print(arguments[0]);
    return _wasmEvalText.apply(null, Array.prototype.slice.apply(arguments, [0]));
}

function assertClose(x, y) {
    assertEq(Math.abs(x - y) < 0.0000000005, true);
}

// "text"

function T(text, args, expected) {
  assertEq(wasmEvalText(text).apply(null, args), expected);
}

// "oneshot function"

function O(fn, args, expected) {
  T(`(module ${fn} (export "" 0))`, args, expected);
}

// Miscellaneous asm.js wrappers where wasm does not expose the
// operator but the baseline compiler supports it.

function asm2(name, import_, decl, op) {
    var argx = decl("x");
    var argy = decl("y");
    var oper = op("(" + argx + "," + argy + ")");
    var code =
    `(function(stdlib, ffi, heap) {
	"use asm";
	var ${name} = ${import_};
	function f(x, y) {
	    x=${argx};
	    y=${argy};
	    return ${oper};
	}
	return { f:f }
    })`;

    return eval(code);
}

function asm(info, m, args, expected, precise) {
    if (VERBOSE)
	print(info);
    var { f } = m(this, {}, new ArrayBuffer(65536));
    (precise ? assertEq : assertClose)(f.apply(null, args), expected);
}

function D2(name, args, expected) {
    asm("D2 Math." + name, asm2(name, `stdlib.Math.${name}`, (x) => "+" + x, (xs) => "+" + name + xs), args, expected, false);
}

function I2(name, args, expected) {
    asm("I2 Math." + name, asm2(name, `stdlib.Math.${name}`, (x) => x + "|0", (xs) => name + xs + "|0"), args, expected, true);
}

function asm1(name, import_, decl, op) {
    var arg = decl("x");
    var oper = op("(" + arg + ")");
    var code =
    `(function(stdlib, ffi, heap) {
	"use asm";
	var ${name} = ${import_};
	function f(x) {
	    x=${arg};
	    return ${oper};
	}
	return { f:f }
    })`;

    return eval(code);
}

function D1(name, args, expected) {
    asm("D1 Math." + name, asm1(name, `stdlib.Math.${name}`, (x) => "+" + x, (xs) => "+" + name + xs), args, expected, false);
}

function I1(name, args, expected) {
    asm("I1 Math." + name, asm1(name, `stdlib.Math.${name}`, (x) => x + "|0", (xs) => name + xs + "|0"), args, expected, false);
}


//////////////////////////////////////////////////////////////////////

// Arithmetic

O("(func (result f64) (f64.add (f64.const 1.5) (f64.const 2.5)))", [], 4);
O("(func (param f64) (param f64) (result f64) (f64.add (get_local 0) (get_local 1)))", [1.5, 2.5], 4);

O("(func (result f32) (f32.add (f32.const 1.5) (f32.const 2.5)))", [], 4);
O("(func (param f32) (param f32) (result f32) (f32.add (get_local 0) (get_local 1)))", [1.5, 2.5], 4);

O("(func (result i32) (i32.add (i32.const 1) (i32.const 2)))", [], 3);
O("(func (param i32) (param i32) (result i32) (i32.add (get_local 0) (get_local 1)))", [1, 2], 3);

O("(func (result f64) (f64.sub (f64.const 1.5) (f64.const 2.5)))", [], -1);
O("(func (param f64) (param f64) (result f64) (f64.sub (get_local 0) (get_local 1)))", [1.5, 2.5], -1);

O("(func (result f32) (f32.sub (f32.const 1.5) (f32.const 2.5)))", [], -1);
O("(func (param f32) (param f32) (result f32) (f32.sub (get_local 0) (get_local 1)))", [1.5, 2.5], -1);

O("(func (result i32) (i32.sub (i32.const 1) (i32.const 2)))", [], -1);
O("(func (param i32) (param i32) (result i32) (i32.sub (get_local 0) (get_local 1)))", [1, 2], -1);

I1("abs", [-3], 3);
I1("abs", [3], 3);

asm("I1 negate", asm1("zappa", "0", (x) => x + "|0", (xs) => "(- " + xs + ")|0"), [1], -1, true);
asm("I1 negate", asm1("zappa", "0", (x) => x + "|0", (xs) => "(- " + xs + ")|0"), [-1], 1, true);

asm("I1 bitnot", asm1("zappa", "0", (x) => x + "|0", (xs) => "(~ " + xs + ")|0"), [1], -2, true);
asm("I1 bitnot", asm1("zappa", "0", (x) => x + "|0", (xs) => "(~ " + xs + ")|0"), [-1], 0, true);

O("(func (param i32) (result i32) (i32.clz (get_local 0)))", [1], 31);
O("(func (param i32) (result i32) (i32.clz (get_local 0)))", [2], 30);
O("(func (param i32) (result i32) (i32.clz (get_local 0)))", [65535], 16);
O("(func (param i32) (result i32) (i32.clz (get_local 0)))", [65535 << 16], 0);

O("(func (param i32) (result i32) (i32.ctz (get_local 0)))", [1], 0);
O("(func (param i32) (result i32) (i32.ctz (get_local 0)))", [2], 1);
O("(func (param i32) (result i32) (i32.ctz (get_local 0)))", [65535], 0);
O("(func (param i32) (result i32) (i32.ctz (get_local 0)))", [65535 << 16], 16);

O("(func (param i32) (result i32) (i32.popcnt (get_local 0)))", [1], 1);
O("(func (param i32) (result i32) (i32.popcnt (get_local 0)))", [2], 1);
O("(func (param i32) (result i32) (i32.popcnt (get_local 0)))", [65535], 16);
O("(func (param i32) (result i32) (i32.popcnt (get_local 0)))", [65537], 2);
O("(func (param i32) (result i32) (i32.popcnt (get_local 0)))", [-1], 32);

O("(func (param i32) (param i32) (result i32) (i32.and (get_local 0) (get_local 1)))", [0x5555, 0x3333], 0x1111);
O("(func (param i32) (param i32) (result i32) (i32.or (get_local 0) (get_local 1)))", [0x5555, 0x3333], 0x7777);
O("(func (param i32) (param i32) (result i32) (i32.xor (get_local 0) (get_local 1)))", [0x5555, 0x3333], 0x6666);

O("(func (param i32) (param i32) (result i32) (i32.shl (get_local 0) (get_local 1)))", [0x5555, 1], 0xaaaa);
O("(func (param i32) (param i32) (result i32) (i32.shr_s (get_local 0) (get_local 1)))", [0x5555, 1], 0x2aaa);
O("(func (param i32) (param i32) (result i32) (i32.shr_s (get_local 0) (get_local 1)))", [-2, 1], -1);
O("(func (param i32) (param i32) (result i32) (i32.shr_u (get_local 0) (get_local 1)))", [-2, 1], (-2 >>> 1));

// Relations

O("(func (param i32) (param i32) (result i32) (i32.eq (get_local 0) (get_local 1)))", [1, 1], 1);
O("(func (param i32) (param i32) (result i32) (i32.eq (get_local 0) (get_local 1)))", [1, 2], 0);

O("(func (param i32) (param i32) (result i32) (i32.ne (get_local 0) (get_local 1)))", [1, 1], 0);
O("(func (param i32) (param i32) (result i32) (i32.ne (get_local 0) (get_local 1)))", [1, 2], 1);

O("(func (param i32) (param i32) (result i32) (i32.lt_s (get_local 0) (get_local 1)))", [1, 1], 0);
O("(func (param i32) (param i32) (result i32) (i32.lt_s (get_local 0) (get_local 1)))", [1, 2], 1);
O("(func (param i32) (param i32) (result i32) (i32.lt_s (get_local 0) (get_local 1)))", [1, -1], 0);
O("(func (param i32) (param i32) (result i32) (i32.lt_u (get_local 0) (get_local 1)))", [1, -1], 1);

O("(func (param i32) (param i32) (result i32) (i32.le_s (get_local 0) (get_local 1)))", [1, 1], 1);
O("(func (param i32) (param i32) (result i32) (i32.le_s (get_local 0) (get_local 1)))", [1, 2], 1);
O("(func (param i32) (param i32) (result i32) (i32.le_s (get_local 0) (get_local 1)))", [1, -1], 0);
O("(func (param i32) (param i32) (result i32) (i32.le_u (get_local 0) (get_local 1)))", [1, -1], 1);

O("(func (param i32) (param i32) (result i32) (i32.gt_s (get_local 0) (get_local 1)))", [1, 1], 0);
O("(func (param i32) (param i32) (result i32) (i32.gt_s (get_local 0) (get_local 1)))", [1, 2], 0);
O("(func (param i32) (param i32) (result i32) (i32.gt_s (get_local 0) (get_local 1)))", [1, -1], 1);
O("(func (param i32) (param i32) (result i32) (i32.gt_u (get_local 0) (get_local 1)))", [1, -1], 0);

O("(func (param i32) (param i32) (result i32) (i32.ge_s (get_local 0) (get_local 1)))", [1, 1], 1);
O("(func (param i32) (param i32) (result i32) (i32.ge_s (get_local 0) (get_local 1)))", [1, 2], 0);
O("(func (param i32) (param i32) (result i32) (i32.ge_s (get_local 0) (get_local 1)))", [1, -1], 1);
O("(func (param i32) (param i32) (result i32) (i32.ge_u (get_local 0) (get_local 1)))", [1, -1], 0);


O("(func (param f32) (param f32) (result i32) (f32.eq (get_local 0) (get_local 1)))", [1, 1], 1);
O("(func (param f32) (param f32) (result i32) (f32.eq (get_local 0) (get_local 1)))", [1, 2], 0);

O("(func (param f32) (param f32) (result i32) (f32.ne (get_local 0) (get_local 1)))", [1, 1], 0);
O("(func (param f32) (param f32) (result i32) (f32.ne (get_local 0) (get_local 1)))", [1, 2], 1);

O("(func (param f32) (param f32) (result i32) (f32.lt (get_local 0) (get_local 1)))", [1, 1], 0);
O("(func (param f32) (param f32) (result i32) (f32.lt (get_local 0) (get_local 1)))", [1, 2], 1);
O("(func (param f32) (param f32) (result i32) (f32.lt (get_local 0) (get_local 1)))", [1, -1], 0);

O("(func (param f32) (param f32) (result i32) (f32.le (get_local 0) (get_local 1)))", [1, 1], 1);
O("(func (param f32) (param f32) (result i32) (f32.le (get_local 0) (get_local 1)))", [1, 2], 1);
O("(func (param f32) (param f32) (result i32) (f32.le (get_local 0) (get_local 1)))", [1, -1], 0);

O("(func (param f32) (param f32) (result i32) (f32.gt (get_local 0) (get_local 1)))", [1, 1], 0);
O("(func (param f32) (param f32) (result i32) (f32.gt (get_local 0) (get_local 1)))", [1, 2], 0);
O("(func (param f32) (param f32) (result i32) (f32.gt (get_local 0) (get_local 1)))", [1, -1], 1);

O("(func (param f32) (param f32) (result i32) (f32.ge (get_local 0) (get_local 1)))", [1, 1], 1);
O("(func (param f32) (param f32) (result i32) (f32.ge (get_local 0) (get_local 1)))", [1, 2], 0);
O("(func (param f32) (param f32) (result i32) (f32.ge (get_local 0) (get_local 1)))", [1, -1], 1);


O("(func (param f64) (param f64) (result i32) (f64.eq (get_local 0) (get_local 1)))", [1, 1], 1);
O("(func (param f64) (param f64) (result i32) (f64.eq (get_local 0) (get_local 1)))", [1, 2], 0);

O("(func (param f64) (param f64) (result i32) (f64.ne (get_local 0) (get_local 1)))", [1, 1], 0);
O("(func (param f64) (param f64) (result i32) (f64.ne (get_local 0) (get_local 1)))", [1, 2], 1);

O("(func (param f64) (param f64) (result i32) (f64.lt (get_local 0) (get_local 1)))", [1, 1], 0);
O("(func (param f64) (param f64) (result i32) (f64.lt (get_local 0) (get_local 1)))", [1, 2], 1);
O("(func (param f64) (param f64) (result i32) (f64.lt (get_local 0) (get_local 1)))", [1, -1], 0);

O("(func (param f64) (param f64) (result i32) (f64.le (get_local 0) (get_local 1)))", [1, 1], 1);
O("(func (param f64) (param f64) (result i32) (f64.le (get_local 0) (get_local 1)))", [1, 2], 1);
O("(func (param f64) (param f64) (result i32) (f64.le (get_local 0) (get_local 1)))", [1, -1], 0);

O("(func (param f64) (param f64) (result i32) (f64.gt (get_local 0) (get_local 1)))", [1, 1], 0);
O("(func (param f64) (param f64) (result i32) (f64.gt (get_local 0) (get_local 1)))", [1, 2], 0);
O("(func (param f64) (param f64) (result i32) (f64.gt (get_local 0) (get_local 1)))", [1, -1], 1);

O("(func (param f64) (param f64) (result i32) (f64.ge (get_local 0) (get_local 1)))", [1, 1], 1);
O("(func (param f64) (param f64) (result i32) (f64.ge (get_local 0) (get_local 1)))", [1, 2], 0);
O("(func (param f64) (param f64) (result i32) (f64.ge (get_local 0) (get_local 1)))", [1, -1], 1);

// Math unary operators

O("(func (result f64) (return (f64.ceil (f64.const 3.14))))", [], 4);
O("(func (result f32) (return (f32.ceil (f32.const 3.14))))", [], 4);

O("(func (result f64) (return (f64.floor (f64.const 3.14))))", [], 3);
O("(func (result f32) (return (f32.floor (f32.const 3.14))))", [], 3);

D1("cos", [0], 1);
D1("sin", [Math.PI/2], 1);
D1("tan", [Math.PI/4], 1);
D1("asin", [1], Math.PI/2);
D1("acos", [1], 0);
D1("atan", [1], Math.PI/4);
D1("log", [Math.E], 1);
D1("exp", [1], Math.E);

// Math binary operators

O("(func (result f64) (f64.max (f64.const 1.5) (f64.const 2.5)))", [], 2.5);
O("(func (result f64) (f64.min (f64.const 1.5) (f64.const 2.5)))", [], 1.5);

O("(func (result f32) (f32.max (f32.const 1.5) (f32.const 2.5)))", [], 2.5);
O("(func (result f32) (f32.min (f32.const 1.5) (f32.const 2.5)))", [], 1.5);

I2("max", [1,2], 2);
I2("min", [1,2], 1);

D2("pow", [3,2], 9);
D2("atan2", [1,1], Math.PI/4);




