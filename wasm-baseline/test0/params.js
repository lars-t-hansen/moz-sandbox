/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

var buffer = new ArrayBuffer(65536);

// Very basic tests for global variables, other tests depend on these
// passing.

// ============================================================

function m_param(stdlib, ffi, heap) {
    "use asm";

    function f(x) {
	x = x|0;
	x = (x + 1)|0;
	return x|0;
    }
    return { f:f };
}

{
    assertEq(isAsmJSModule(m_param), true);
    let { f } = m_param(this, {}, buffer);
    assertEq(f(), 1);
}
