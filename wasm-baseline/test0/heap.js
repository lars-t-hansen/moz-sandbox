/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

var buffer = new ArrayBuffer(65536);

// Tests for heap access.

// ============================================================

function m_load(stdlib, ffi, heap) {
    "use asm";

    var i32a = new stdlib.Int32Array(heap);
    var f64a = new stdlib.Float64Array(heap);

    function f(k) {
	k = k|0;
	var x = 0;
	x = i32a[k >> 2]|0;
	return x|0;
    }
    function g(k) {
	k = k|0;
	var x = 0.0;
	x = +f64a[k >> 3];
	return +x;
    }
    return { f:f, g:g };
}

{
    assertEq(isAsmJSModule(m_load), true);
    let { f, g } = m_load(this, {}, buffer);

    // int32 access

    let ia = new Int32Array(buffer);
    ia[0] = 37;
    ia[1] = 42;
    ia[3] = 0xdeadbeef;
    ia[ia.length-1] = 0xcafebabe;

    assertEq(f(0), 37);
    assertEq(f(4), 42);
    assertEq(f(8), 0);
    assertEq(f(12), 0xdeadbeef|0);
    assertEq(f(buffer.byteLength-4), 0xcafebabe|0);

    // oob access should be zero, I think
    assertEq(f(buffer.byteLength), 0);
    assertEq(f(-1 << 2), 0);

    // float64 access

    let da = new Float64Array(buffer);
    da[20] = 1.5;
    da[21] = 33.75;

    assertEq(g(20*8), 1.5);
    assertEq(g(21*8), 33.75);

    assertEq(g(buffer.byteLength), NaN);
    assertEq(g(-1 << 2), NaN);
}

// ============================================================

function m_store(stdlib, ffi, heap) {
    "use asm";

    var i32a = new stdlib.Int32Array(heap);
    var f64a = new stdlib.Float64Array(heap);

    function f(k, v) {
	k = k|0;
	v = v|0;
	i32a[k >> 2] = (v|0);
    }

    function g(k, v) {
	k = k|0;
	v = +v;
	f64a[k >> 3] = +v;
    }

    return { f:f, g:g };
}

{
    assertEq(isAsmJSModule(m_store), true);
    let { f, g } = m_store(this, {}, buffer);

    // int32 access

    let ia = new Int32Array(buffer);
    f(0, 98);
    assertEq(ia[0], 98);
    f(4, 133);
    assertEq(ia[1], 133);
    f(12, 0xdeadbeef|0);
    assertEq(ia[3], 0xdeadbeef|0);
    f(buffer.byteLength-4, 0xcafebabe|0);
    assertEq(ia[ia.length-1], 0xcafebabe|0);

    // Should be unchanged
    assertEq(ia[2], 0);

    // oob access should at least not crash
    f(buffer.byteLength, 0);
    f(-1 << 2, 0);

    // float64 access
    let da = new Float64Array(buffer);
    g(20*8, 3.125);
    assertEq(da[20], 3.125);
    g(21*8, -18.5);
    assertEq(da[21], -18.5);

    // oob
    g(buffer.byteLength, 0);
    g(-1 << 3, 0);
}
