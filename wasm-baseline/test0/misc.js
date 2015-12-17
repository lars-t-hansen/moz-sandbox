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

