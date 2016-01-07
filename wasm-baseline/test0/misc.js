/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

var buffer = new ArrayBuffer(65536);

// ============================================================

function m_comma(stdlib, ffi, heap) {
    "use asm";

    var v = 10;
    var w = 20;
    var x = 7;

    function f() {
	return (v, w, 1, 2, x)|0;
    }
    return { f: f }
}

{
    assertEq(isAsmJSModule(m_comma), true);
    let { f } = m_comma(this, {}, buffer);
    assertEq(f(), 7);
}

// ============================================================

function m_minmax(stdlib, ffi, heap) {
    "use asm";

    var max = stdlib.Math.max;
    var min = stdlib.Math.min;

    function fmax(a, b) {
	a = a|0;
	b = b|0;
	return max(a|0, b|0)|0;
    }
    function fmax5(a, b, c, d, e) {
	a = a|0;
	b = b|0;
	c = c|0;
	d = d|0;
	e = e|0;
	return max(a|0, b|0, c|0, d|0, e|0)|0;
    }
    function fmin(a, b) {
	a = a|0;
	b = b|0;
	return min(a|0, b|0)|0;
    }
    function fmin5(a, b, c, d, e) {
	a = a|0;
	b = b|0;
	c = c|0;
	d = d|0;
	e = e|0;
	return min(a|0, b|0, c|0, d|0, e|0)|0;
    }
    return { fmax:fmax, fmin:fmin, fmax5:fmax5, fmin5:fmin5 }
}

{
    assertEq(isAsmJSModule(m_minmax), true);
    let { fmax, fmin, fmax5, fmin5 } = m_minmax(this, {}, buffer);
    assertEq(fmax(1, 5), 5);
    assertEq(fmax(5, 1), 5);
    assertEq(fmax(-1, -5), -1);
    assertEq(fmax(-5, -1), -1);
    assertEq(fmin(1, 5), 1);
    assertEq(fmin(5, 1), 1);
    assertEq(fmin(-1, -5), -5);
    assertEq(fmin(-5, -1), -5);
    assertEq(fmax5(-5, -1, 5, 1, 3), 5);
    assertEq(fmin5(5, 1, -5, -1, -3), -5);
}

function m_minmax_d(stdlib, ffi, heap) {
    "use asm";

    var max = stdlib.Math.max;
    var min = stdlib.Math.min;

    function fmax(a, b) {
	a = +a;
	b = +b;
	return +max(a, b);
    }
    function fmax5(a, b, c, d, e) {
	a = +a;
	b = +b;
	c = +c;
	d = +d;
	e = +e;
	return +max(a, b, c, d, e);
    }
    function fmin(a, b) {
	a = +a;
	b = +b;
	return +min(a, b);
    }
    function fmin5(a, b, c, d, e) {
	a = +a;
	b = +b;
	c = +c;
	d = +d;
	e = +e;
	return +min(a, b, c, d, e);
    }
    return { fmax:fmax, fmin:fmin, fmax5:fmax5, fmin5:fmin5 }
}

{
    assertEq(isAsmJSModule(m_minmax_d), true);
    let { fmax, fmin, fmax5, fmin5 } = m_minmax_d(this, {}, buffer);
    assertEq(fmax(1, 5), 5);
    assertEq(fmax(5, 1), 5);
    assertEq(fmax(-1, -5), -1);
    assertEq(fmax(-5, -1), -1);
    assertEq(fmin(1, 5), 1);
    assertEq(fmin(5, 1), 1);
    assertEq(fmin(-1, -5), -5);
    assertEq(fmin(-5, -1), -5);
    assertEq(fmax5(-5, -1, 5, 1, 3), 5);
    assertEq(fmin5(5, 1, -5, -1, -3), -5);
}

function m_minmax_f(stdlib, ffi, heap) {
    "use asm";

    var max = stdlib.Math.max;
    var min = stdlib.Math.min;
    var F = stdlib.Math.fround;

    function fmax(a, b) {
	a = F(a);
	b = F(b);
	return F(max(a, b));
    }
    function fmax5(a, b, c, d, e) {
	a = F(a);
	b = F(b);
	c = F(c);
	d = F(d);
	e = F(e);
	return F(max(a, b, c, d, e));
    }
    function fmin(a, b) {
	a = F(a);
	b = F(b);
	return F(min(a, b));
    }
    function fmin5(a, b, c, d, e) {
	a = F(a);
	b = F(b);
	c = F(c);
	d = F(d);
	e = F(e);
	return F(min(a, b, c, d, e));
    }
    return { fmax:fmax, fmin:fmin, fmax5:fmax5, fmin5:fmin5 }
}

{
    assertEq(isAsmJSModule(m_minmax_f), true);
    let { fmax, fmin, fmax5, fmin5 } = m_minmax_f(this, {}, buffer);
    assertEq(fmax(1, 5), 5);
    assertEq(fmax(5, 1), 5);
    assertEq(fmax(-1, -5), -1);
    assertEq(fmax(-5, -1), -1);
    assertEq(fmin(1, 5), 1);
    assertEq(fmin(5, 1), 1);
    assertEq(fmin(-1, -5), -5);
    assertEq(fmin(-5, -1), -5);
    assertEq(fmax5(-5, -1, 5, 1, 3), 5);
    assertEq(fmin5(5, 1, -5, -1, -3), -5);
}
