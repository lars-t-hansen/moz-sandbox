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

// Pow only defined for F64
function m_pow_d(stdlib, ffi, heap) {
    "use asm";

    var pow = stdlib.Math.pow;

    function f(x, y) {
	x = +x;
	y = +y;
	return +pow(x, y);
    }

    return { f:f }
}

{
    assertEq(isAsmJSModule(m_pow_d), true);
    let { f } = m_pow_d(this, {}, buffer);
    assertEq(f(3.5, 2), 12.25);
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

function m_sqrt(stdlib, ffi, heap) {
    "use asm";

    var fround = stdlib.Math.fround;
    var sqrt = stdlib.Math.sqrt;

    function f(x) {
	x = +x;
	return +sqrt(x);
    }

    function g(x) {
	x = fround(x);
	return fround(sqrt(x));
    }

    return { f:f, g:g };
}

{
    assertEq(isAsmJSModule(m_sqrt), true);
    let { f, g } = m_sqrt(this, {}, buffer);

    assertEq(f(3.5), Math.sqrt(3.5));

    assertEq(g(4), 2);
}

function m_abs(stdlib, ffi, heap) {
    "use asm";

    var fround = stdlib.Math.fround;
    var abs = stdlib.Math.abs;

    function f(x) {
	x = +x;
	return +abs(x);
    }

    function g(x) {
	x = fround(x);
	return fround(abs(x));
    }

    function h(x) {
	x = x|0;
	return abs(x|0)|0;
    }

    return { f:f, g:g, h:h };
}

{
    assertEq(isAsmJSModule(m_abs), true);
    let { f, g, h } = m_abs(this, {}, buffer);

    assertEq(f(3.5), 3.5);
    assertEq(f(-3.5), 3.5);
    assertEq(f(-0), 0);

    assertEq(g(-4.5), 4.5);
    assertEq(g(4.5), 4.5);
    assertEq(g(-0), 0);

    assertEq(h(-4), 4);
    assertEq(h(4), 4);
    assertEq(h(-0), 0);
}

function m_clz(stdlib, ffi, heap) {
    "use asm";

    var clz = stdlib.Math.clz32;

    function f(x) {
	x = x|0;
	return clz(x|0)|0;
    }

    return { f:f };
}

{
    assertEq(isAsmJSModule(m_clz), true);
    let { f } = m_clz(this, {}, buffer);

    assertEq(f(1), 31);
    assertEq(f(-1), 0);
    assertEq(f(0), 32);
}
