/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

var buffer = new ArrayBuffer(65536);

function m_call_direct(stdlib, ffi, heap) {
    "use asm";

    function f(x) {
	x = x|0;
	return ((g(x)|0) + (h(x)|0))|0;
    }
    function g(x) {
	x = x|0;
	return (x+10)|0;
    }
    function h(x) {
	x = x|0;
	return (x-5)|0;
    }
    return { f:f };
}

{
    assertEq(isAsmJSModule(m_call_direct), true);
    let { f } = m_call_direct(this, {}, buffer);
    assertEq(f(3), 8);
}

