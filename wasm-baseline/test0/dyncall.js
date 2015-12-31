/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

var buffer = new ArrayBuffer(65536);

function m_dyncall(stdlib, ffi, heap) {
    "use asm";

    function f(k, n) {
	k = k|0;
	n = +n;
	return +(vtable[k & 3](n));
    }

    function f1(n) {
	n = +n;
	return +(n + 1.0);
    }

    function f2(n) {
	n = +n;
	return +(-n);
    }

    function f3(n) {
	n = +n;
	return +(n - 3.5);
    }

    function f4(n) {
	n = +n;
	return +(n + n);
    }

    var vtable = [ f1, f2, f3, f4 ];

    return { f:f };
}

{
    assertEq(isAsmJSModule(m_dyncall), true);
    let { f } = m_dyncall(this, {}, buffer);
    assertEq(f(0, 7.5), 8.5);
    assertEq(f(1, 7.5), -7.5);
    assertEq(f(2, 7.5), 4);
    assertEq(f(3, 7.5), 15);
}
