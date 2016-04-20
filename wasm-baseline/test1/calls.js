var libdir = "../../../mozilla-inbound/js/src/jit-test/lib/"

load(libdir + "wasm.js");

assertEq(wasmEvalText(
    `(module
      (func $f (param i32) (result i32)
       (return (i32.add (get_local 0) (i32.const 5))))
      (func $g (param i32) (result i32)
       (return (call $f (get_local 0))))
      (export "" 1))`)(10), 15);

assertEq(wasmEvalText(
    `(module
      (func $f (param i32) (param i32) (result i32)
       (return (i32.sub (get_local 0) (get_local 1))))
      (func $g (param i32) (param i32) (result i32)
       (return (call $f (get_local 1) (get_local 0))))
      (export "" 1))`)(10, 12), 2);

var m = wasmEvalText(
`(module
  (type $t (func (param f64) (result f64)))
  (func (type $t) (return (get_local 0)))
  (func (type $t) (return (f64.add (get_local 0) (f64.const 1))))
  (table 0 1)
  (func (result f64) (return (call_indirect $t (i32.const 1) (f64.const 3.5))))
  (export "" 2))`);

assertEq(m(), 4.5);
