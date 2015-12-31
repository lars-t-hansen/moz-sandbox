/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

var buffer = new ArrayBuffer(65536);

// Tests for local variables.

// ============================================================

function m_local_var(stdlib, ffi, heap) {
    "use asm";

    function f() {
	var x = 37;
	var y = 42;
	var z = 12;
	x = (x + 1)|0;
	return x|0;
    }
    function g() {
	var x = 37;
	var y = 42;
	var z = 12;
	y = (y + 1)|0;
	return x|0;		// sic
    }
    function h() {
	var x = 37;
	var y = 42;
	var z = 12;
	z = (z + 1)|0;
	return y|0;		// sic
    }
    return { f:f, g:g, h:h };
}

{
    assertEq(isAsmJSModule(m_local_var), true);
    let { f, g, h } = m_local_var(this, {}, buffer);
    assertEq(f(), 38);
    assertEq(g(), 37);
    assertEq(h(), 42);
}

function m_local_var_d(stdlib, ffi, heap) {
    "use asm";

    function f() {
	var x = 37.25;
	var y = 42.1;
	var z = 12.5;
	x = +(x + 1.4);
	return +x;
    }
    function g() {
	var x = 37.25;
	var y = 42.1;
	var z = 12.5;
	y = +(y + 1.4);
	return +x;		// sic
    }
    function h() {
	var x = 37.25;
	var y = 42.1;
	var z = 12.5;
	z = +(z + 1.4);
	return +y;		// sic
    }
    return { f:f, g:g, h:h };
}

{
    assertEq(isAsmJSModule(m_local_var_d), true);
    let { f, g, h } = m_local_var_d(this, {}, buffer);
    assertEq(f(), 37.25 + 1.4);
    assertEq(g(), 37.25);
    assertEq(h(), 42.1);
}

function m_local_var_f(stdlib, ffi, heap) {
    "use asm";

    var F = stdlib.Math.fround;

    function f() {
	var x = F(37.25);
	var y = F(42.5);
	var z = F(12.5);
	x = F(x + F(1.25));
	return F(x);
    }
    function g() {
	var x = F(37.25);
	var y = F(42.5);
	var z = F(12.5);
	y = F(y + F(1.25));
	return F(x);		// sic
    }
    function h() {
	var x = F(37.25);
	var y = F(42.5);
	var z = F(12.5);
	z = F(z + F(1.25));
	return F(y);		// sic
    }
    return { f:f, g:g, h:h };
}

{
    assertEq(isAsmJSModule(m_local_var_f), true);
    let { f, g, h } = m_local_var_f(this, {}, buffer);
    assertEq(f(), 37.25 + 1.25);
    assertEq(g(), 37.25);
    assertEq(h(), 42.5);
}
