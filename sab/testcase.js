function f(stdlib, ffi, heap) {
    "use asm";

    var a = stdlib.Int32Array;
    var b = stdlib.SharedUint8Array;
    var x = new a(heap);
    //var y = new b(heap);

    function g() {
	return 0;
    }

    return g;
}

var x = new ArrayBuffer(65536);
print(f(this, null, x)());

