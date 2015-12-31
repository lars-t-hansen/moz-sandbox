/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

var buffer = new ArrayBuffer(65536);

function m_floor_ceil_d(stdlib, ffi, heap) {
    "use asm";

    var floor = stdlib.Math.floor;
    var ceil = stdlib.Math.ceil;

    function f(x) {
	x = +x;
	return +floor(x);
    }

    function c(x) {
	x = +x;
	return +ceil(x);
    }

    return { f:f, c:c }
}

{
    assertEq(isAsmJSModule(m_floor_ceil_d), true);
    let { f, c } = m_floor_ceil_d(this, {}, buffer);
    assertEq(f(3.5), 3);
    assertEq(c(3.5), 4);
}


function m_floor_ceil_f(stdlib, ffi, heap) {
    "use asm";

    var fround = stdlib.Math.fround;
    var floor = stdlib.Math.floor;
    var ceil = stdlib.Math.ceil;

    function f(x) {
	x = fround(x);
	return fround(floor(x));
    }

    function c(x) {
	x = fround(x);
	return fround(ceil(x));
    }

    return { f:f, c:c }
}

{
    assertEq(isAsmJSModule(m_floor_ceil_f), true);
    let { f, c } = m_floor_ceil_f(this, {}, buffer);
    assertEq(f(3.5), 3);
    assertEq(c(3.5), 4);
}

