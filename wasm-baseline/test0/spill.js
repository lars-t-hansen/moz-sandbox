/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

var buffer = new ArrayBuffer(65536);

function m_spill(stdlib, ffi, heap) {
    "use asm";

    var x = 10;

    function f() {
	// Enough deferred computation to force register spilling on
	// x64 (three spills) and almost certainly on ARM32 (not
	// investigated, but should spill more than that).
	//
	// For MIPS and ARM64 we might need more.
	return ((x + ((x + ((x + ((x + ((x + ((x + ((x + ((x + ((x + ((x + ((x + ((x + ((x + ((x + ((x + x)|0))|0))|0))|0))|0))|0))|0))|0))|0))|0))|0))|0))|0))|0))|0);
    }
    return { f:f };
}

{
    assertEq(isAsmJSModule(m_spill), true);
    let { f } = m_spill(this, {}, buffer);
    assertEq(f(), 10*16);
}

function m_spill_d(stdlib, ffi, heap) {
    "use asm";

    var x = 10.5;

    function f() {
	return +(x + +(x + +(x + +(x + +(x + +(x + +(x + +(x + +(x + +(x + +(x + +(x + +(x + +(x + +(x + +(x + +(x + +(x + +(x + x)))))))))))))))))));
    }
    return { f:f };
}

{
    assertEq(isAsmJSModule(m_spill_d), true);
    let { f } = m_spill_d(this, {}, buffer);
    assertEq(f(), 10.5*20);
}

function m_spill_f(stdlib, ffi, heap) {
    "use asm";

    var F = stdlib.Math.fround;
    var x = F(10.5);

    function f() {
	return F(x + F(x + F(x + F(x + F(x + F(x + F(x + F(x + F(x + F(x + F(x + F(x + F(x + F(x + F(x + F(x + F(x + F(x + F(x + x)))))))))))))))))));
    }
    return { f:f };
}

{
    assertEq(isAsmJSModule(m_spill_f), true);
    let { f } = m_spill_f(this, {}, buffer);
    assertEq(f(), 10.5*20);
}
