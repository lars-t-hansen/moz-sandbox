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
