/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

var buffer = new ArrayBuffer(65536);

function m_fficall(stdlib, ffi, heap) {
    "use asm";

    var process = ffi.process;
    var mangle = ffi.mangle;

    function f(x, y) {
	x = x|0;
	y = +y;
	return +mangle(+process(x|0, +y) + +mangle(1.0));
    }
    return { f:f };
}

{
    assertEq(isAsmJSModule(m_fficall), true);
    let { f } = m_fficall(this, { process: (a, b) => a+b, mangle: x => -x }, buffer);
    assertEq(f(3, 7.5), -9.5);
}
