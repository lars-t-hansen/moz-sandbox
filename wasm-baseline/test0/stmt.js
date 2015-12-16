/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

var buffer = new ArrayBuffer(65536);

// ============================================================

function m_if_then(stdlib, ffi, heap) {
    "use asm";

    var v = 10;

    function f() {
	if ((v|0) > 5)
	    v = 20;
	return v|0;
    }
    function g() {
	if ((v|0) >= 20)
	    v = 30;
	return v|0;
    }
    return { f:f, g:g };
}

{
    assertEq(isAsmJSModule(m_if_then), true);
    let { f, g } = m_if_then(this, {}, buffer);
    assertEq(f(), 20);
    assertEq(g(), 30);
}

function m_if_else(stdlib, ffi, heap) {
    "use asm";

    var v = 10;

    function f() {
	if ((v|0) < 5)
	    v = 5;
	else if ((v|0) == 3)
	    v = 10;
	else
	    v = 20;
	return v|0;
    }
    function g() {
	if ((v|0) > 20)
	    v = 312;
	else if ((v|0) == 8)
	    v = 33;
	else
	    v = 10;
	return v|0;
    }
    return { f:f, g:g };
}

{
    assertEq(isAsmJSModule(m_if_else), true);
    let { f, g } = m_if_else(this, {}, buffer);
    assertEq(f(), 20);
    assertEq(g(), 10);
}

// ============================================================

function m_block(stdlib, ffi, heap) {
    "use asm";

    var v = 10;

    function f() {
	if ((v|0) > 5) {
	    v = ((v|0) + 20)|0;
	    v = (v|0) | 1;
	}
	return v|0;		// Should be 31
    }
    return { f:f };
}

{
    assertEq(isAsmJSModule(m_block), true);
    let { f } = m_block(this, {}, buffer);
    assertEq(f(), 31);
}

// ============================================================

function m_while(stdlib, ffi, heap) {
    "use asm";

    var v = 0;
    var w = 0;

    function f() {
	while ((v|0) < 10) {
	    w = ((w|0) + 2)|0;
	    v = ((v|0) + 1)|0;
	}
	return w|0;
    }
    return { f:f };
}

{
    assertEq(isAsmJSModule(m_while), true);
    let { f } = m_while(this, {}, buffer);
    assertEq(f(), 20);
}

function m_while_break(stdlib, ffi, heap) {
    "use asm";

    var v = 0;
    var w = 0;

    function f() {
	while ((v|0) < 10) {
	    w = ((w|0) + 2)|0;
	    v = ((v|0) + 1)|0;
	    if ((v|0) == 5)
		break;
	    w = ((w|0) - 1)|0;
	}
	return w|0;
    }
    return { f:f };
}

{
    assertEq(isAsmJSModule(m_while_break), true);
    let { f } = m_while_break(this, {}, buffer);
    assertEq(f(), 6);
}

function m_while_break_label(stdlib, ffi, heap) {
    "use asm";

    var v = 0;
    var w = 0;

    function f() {
	outer:
	middle:
	inner:
	while (1) {
	    while ((v|0) < 10) {
		w = ((w|0) + 2)|0;
		v = ((v|0) + 1)|0;
		if ((v|0) == 5)
		    break middle;
		w = ((w|0) - 1)|0;
	    }
	}
	return w|0;
    }
    return { f:f };
}

{
    assertEq(isAsmJSModule(m_while_break_label), true);
    let { f } = m_while_break_label(this, {}, buffer);
    assertEq(f(), 6);
}

function m_while_continue(stdlib, ffi, heap) {
    "use asm";

    var v = 0;
    var w = 0;

    function f() {
	while ((v|0) < 10) {
	    w = ((w|0) + 2)|0;
	    v = ((v|0) + 1)|0;
	    if ((v|0) == 5)
		continue;
	    w = ((w|0) - 1)|0;
	}
	return w|0;
    }
    return { f:f };
}

{
    assertEq(isAsmJSModule(m_while_continue), true);
    let { f } = m_while_continue(this, {}, buffer);
    assertEq(f(), 11);
}

function m_while_continue_label(stdlib, ffi, heap) {
    "use asm";

    var v = 0;
    var w = 0;
    var x = 1;

    function f() {
	outer:
	middle:
	inner:
	while (x|0) {
	    x = 0;
	    while ((v|0) < 10) {
		w = ((w|0) + 2)|0;
		v = ((v|0) + 1)|0;
		if ((v|0) == 5)
		    continue middle;
		w = ((w|0) - 1)|0;
	    }
	}
	return w|0;
    }
    return { f:f };
}

{
    assertEq(isAsmJSModule(m_while_continue_label), true);
    let { f } = m_while_continue_label(this, {}, buffer);
    assertEq(f(), 6);
}

// ============================================================

function m_do(stdlib, ffi, heap) {
    "use asm";

    var v = 0;
    var w = 0;

    function f() {
	do {
	    w = ((w|0) + 2)|0;
	    v = ((v|0) + 1)|0;
	} while ((v|0) < 10);
	return w|0;
    }
    return { f:f };
}

{
    assertEq(isAsmJSModule(m_do), true);
    let { f } = m_do(this, {}, buffer);
    assertEq(f(), 20);
}

function m_do_break(stdlib, ffi, heap) {
    "use asm";

    var v = 0;
    var w = 0;

    function f() {
	do {
	    w = ((w|0) + 2)|0;
	    v = ((v|0) + 1)|0;
	    if ((v|0) == 5)
		break;
	    w = ((w|0) - 1)|0;
	} while ((v|0) < 10);
	return w|0;
    }
    return { f:f };
}

{
    assertEq(isAsmJSModule(m_do_break), true);
    let { f } = m_do_break(this, {}, buffer);
    assertEq(f(), 6);
}

function m_do_break_label(stdlib, ffi, heap) {
    "use asm";

    var v = 0;
    var w = 0;

    function f() {
	outer:
	middle:
	inner:
	do {
	    do {
		w = ((w|0) + 2)|0;
		v = ((v|0) + 1)|0;
		if ((v|0) == 5)
		    break middle;
		w = ((w|0) - 1)|0;
	    } while ((v|0) < 10);
	} while(1);
	return w|0;
    }
    return { f:f };
}

{
    assertEq(isAsmJSModule(m_do_break_label), true);
    let { f } = m_do_break_label(this, {}, buffer);
    assertEq(f(), 6);
}

function m_do_continue(stdlib, ffi, heap) {
    "use asm";

    var v = 0;
    var w = 0;

    function f() {
	do {
	    w = ((w|0) + 2)|0;
	    v = ((v|0) + 1)|0;
	    if ((v|0) == 5)
		continue;
	    w = ((w|0) - 1)|0;
	} while ((v|0) < 10);
	return w|0;
    }
    return { f:f };
}

{
    assertEq(isAsmJSModule(m_do_continue), true);
    let { f } = m_do_continue(this, {}, buffer);
    assertEq(f(), 11);
}

function m_do_continue_label(stdlib, ffi, heap) {
    "use asm";

    var v = 0;
    var w = 0;
    var x = 2;
    var z = 0;

    function f() {
	outer:
	middle:
	inner:
	do {
	    z = ((z|0) + 1)|0;
	    x = ((x|0) - 1)|0;
	    do {
		w = ((w|0) + 2)|0;
		v = ((v|0) + 1)|0;
		if ((v|0) >= 5)
		    continue middle;
		w = ((w|0) - 1)|0;
	    } while ((v|0) < 10);
	    x = 100;
	} while (x|0);
	return ((w|0) + (z|0))|0;
    }
    return { f:f };
}

{
    assertEq(isAsmJSModule(m_do_continue_label), true);
    let { f } = m_do_continue_label(this, {}, buffer);
    assertEq(f(), 10);
}

// ============================================================

function m_for(stdlib, ffi, heap) {
    "use asm";

    var v = 0;
    var w = 0;

    function f() {
	for ( v=0 ; (v|0) < 10 ; v=((v|0)+1)|0 ) {
	    w = ((w|0) + 2)|0;
	}
	return w|0;
    }
    function g() {
	v=0;
	w=0;
	for ( ; (v|0) < 10 ; v=((v|0)+1)|0 ) {
	    w = ((w|0) + 2)|0;
	}
	return w|0;
    }
    return { f:f, g:g };
}

{
    assertEq(isAsmJSModule(m_for), true);
    let { f, g } = m_for(this, {}, buffer);
    assertEq(f(), 20);
    assertEq(g(), 20);
}

function m_for_continue(stdlib, ffi, heap) {
    "use asm";

    var v = 0;
    var w = 0;

    function f() {
	for ( v=0 ; (v|0) < 10 ; v=((v|0)+1)|0 ) {
	    w = ((w|0) + 2)|0;
	    if ((v|0) >= 5)
		continue;
	    w = ((w|0) - 1)|0;
	}
	return w|0;
    }
    return { f:f };
}

{
    assertEq(isAsmJSModule(m_for_continue), true);
    let { f } = m_for_continue(this, {}, buffer);
    assertEq(f(), 15);
}
