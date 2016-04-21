var libdir = "../../../mozilla-inbound/js/src/jit-test/lib/"

load(libdir + "wasm.js");

var _wasmEvalText = wasmEvalText;

wasmEvalText = function () {
    print(arguments[0]);
    return _wasmEvalText.apply(null, Array.prototype.slice.apply(arguments, [0]));
}

assertEq(wasmEvalText(`(module (func (result f64) (return (f64.ceil (f64.const 3.14)))) (export "" 0))`)(), 4);

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
assertEq(f(3,2), 9);



