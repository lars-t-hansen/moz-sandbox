var libdir = "../../../mozilla-inbound/js/src/jit-test/lib/"

load(libdir + "wasm.js")

// Traditionally, a smalltalk implementation works when 3+4 = 7

/*
assertEq(wasmEvalText(
    `(module
      (func $smalltalk (result i32)
       (i32.add
	(i32.add (i32.add (i32.const 3) (i32.const 4)) (i32.add (i32.const 5) (i32.const 7)))
	(i32.add (i32.add (i32.const 3) (i32.const 2)) (i32.add (i32.const 5) (i32.const 6)))))
      (export "" 0))`)(), 35);

assertEq(wasmEvalText(
    `(module
      (func $smalltalk (result f64)
       (f64.add
	(f64.add (f64.add (f64.const 3) (f64.const 4)) (f64.add (f64.const 5) (f64.const 7)))
	(f64.add (f64.add (f64.const 3) (f64.const 2)) (f64.add (f64.const 5) (f64.const 6)))))
      (export "" 0))`)(), 35);

assertEq(wasmEvalText(
    `(module
      (func $smalltalk (result i32)
       (local i32)
       (local i32)
       (set_local 0 (i32.add (i32.add (i32.const 3) (i32.const 4)) (i32.add (i32.const 5) (i32.const 7))))
       (set_local 1 (i32.add (i32.add (i32.const 3) (i32.const 2)) (i32.add (i32.const 5) (i32.const 6))))
       (i32.add (get_local 0) (get_local 1)))
      (export "" 0))`)(), 35);

assertEq(wasmEvalText(
    `(module
      (func $smalltalk (result f64)
       (local f64)
       (local f64)
       (set_local 0 (f64.add (f64.add (f64.const 3) (f64.const 4)) (f64.add (f64.const 5) (f64.const 7))))
       (set_local 1 (f64.add (f64.add (f64.const 3) (f64.const 2)) (f64.add (f64.const 5) (f64.const 6))))
       (f64.add (get_local 0) (get_local 1)))
      (export "" 0))`)(), 35);
*/

/*
var m = wasmEvalText(
    `(module
      (func $fac-opt (param i32) (result i32)
       (local i32)
       (set_local 1 (i32.const 1))
       (block
	(i32.add
	 (i32.const 7)
	 (block
	  (br_if 1 (get_local 0))
	  (set_local 1 (i32.const 2)))))
       (get_local 1))

      (export "" 0))`);

assertEq(m(0), 2);
assertEq(m(1), 1);
*/

//var f = wasmEvalText(`(module (func (param i32) (result i32) (i32.add (i32.const 1) (block (if (get_local 0) (br 1 (i32.const 99))) (i32.const -1)))) (export "" 0))`);

/*
assertEq(wasmEvalText(
    `(module
      (func
       (result i32)
       (if (i32.const 42)
	   (block
	    (if (block (i32.const 3) (i32.const 5) (i32.const 0))
		(i32.const 1)
	        (i32.const 2)))
	   (i32.const 4)))
      (export "" 0))`)(), 2);

assertEq(wasmEvalText(
    `(module
      (func
       (result i32)
       (if (i32.const 1)
           (block (br 1) (unreachable))
           (block (unreachable)))
       (i32.const 1))
      (export "" 0))`)(), 1);
*/

//assertEq(wasmEvalText('(module (func (result i32) (if (i32.const 42) (i32.const 1) (i32.const 2))) (export "" 0))')(), 1);

//assertEq(wasmEvalText('(module (func (result i32) (block (br 0 (i32.const 42)) (i32.const 13))) (export "" 0))')(), 42);

//var f = wasmEvalText(`(module (func (result i32) (param i32) (block (br_if 0 (i32.const 42) (get_local 0)) (br 0 (i32.const 43)))) (export "" 0))`);
//assertEq(f(0), 43);
//assertEq(f(1), 42);



/*
assertEq(wasmEvalText(`(module
 (func (result i32)
  (local i32)
  (loop $out $in
   (set_local 0 (i32.add (get_local 0) (i32.const 1)))
   (br_if $out (get_local 0) (i32.ge_s (get_local 0) (i32.const 7)))
   (br $in)
  )
 )
(export "" 0))`)(), 7);
*/


//wasmEvalText('(module (func (local i32) (if (get_local 0) (nop) (nop))) (export "" 0))');

/*
assertEq(wasmEvalText(`(module
    (func
        (result i32)
        (if
            (i32.const 42)
            (i32.const 1)
            (unreachable)
        )
    )
    (export "" 0)
)`)(), 1);
*/

//assertEq(wasmEvalText('(module (func (result i32) (return (i32.const 1))) (export "" 0))')(), 1);

//assertEq(wasmEvalText('(module (func (if (return) (i32.const 0))) (export "" 0))')(), undefined);

/*
var f = wasmEvalText(`(module (func (result i32) (param i32)
  (block $0
   (block $1
    (block $2
     (block $default
      (br_table $0 $1 $2 $default (get_local 0))
     )
     (return (i32.const -1))
    )
    (return (i32.const 2))
   )
  )
  (return (i32.const 0))
) (export "" 0))`);
*/

/*
assertEq(wasmEvalText(`(module (func (result i32) (param i32)
  (block $default
   (br_table $default (get_local 0))
   (return (i32.const 0))
  )
  (return (i32.const 1))
) (export "" 0))`)(), 1);
*/

//assertEq(wasmEvalText('(module (func (block (block (br 1)))) (export "" 0))')(), undefined);

/*
var m = wasmEvalText(
`(module
  (type $t (func (param f64) (result f64)))
  (func (type $t) (return (get_local 0)))
  (func (type $t) (return (f64.add (get_local 0) (f64.const 1))))
  (table 0 1)
  (func (result f64) (return (call_indirect $t (i32.const 1) (f64.const 3.5))))
  (export "" 2))`);

assertEq(m(), 4.5);
*/

/*
var v2i = wasmEvalText(`(module
    (type (func (result i32)))
    (func (type 0) (i32.const 13))
    (func (type 0) (i32.const 42))
    (table 0 1)
    (func (param i32) (result i32) (call_indirect 0 (get_local 0)))
    (export "v2i" 2)
)`);
*/

/*
print(wasmEvalText(`(module (func (result f64) (return (f64.ceil (f64.const 3.14)))) (export "" 0))`)());

function m(stdlib, ffi, heap) {
    "use asm";
    var pow = stdlib.Math.pow;
    function f(x, y) {
	x=+x;
	y=+y;
	return +pow(x,y);
    }
    return { f:f }
}

var { f } = m(this, {}, new ArrayBuffer(65536));
print(f(3,2));
*/

assertEq(wasmEvalText(`(module
 (func
  (result i32)
  (local i32)
  (loop $out $in
   (set_local 0 (i32.add (get_local 0) (i32.const 1)))
   (br_if $out (get_local 0) (i32.ge_s (get_local 0) (i32.const 7)))
   (br $in)
  )
 )
(export "" 0))`)(), 7);

