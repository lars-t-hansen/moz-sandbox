/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

var buffer = new ArrayBuffer(65536);

// The asm-to-wasm translator performs no constant folding, so the
// test (literal OP literal) actually test code generation for OP.

// ============================================================

function m_add_literals(stdlib, ffi, heap) {
    "use asm";

    function f() {
	return (37 + 42)|0;
    }
    return { f:f };
}

{
    assertEq(isAsmJSModule(m_add_literals), true);
    let { f } = m_add_literals(this, {}, buffer);
    assertEq(f(), 37 + 42);
}

function m_add_global_literal(stdlib, ffi, heap) {
    "use asm";

    var g = 37;

    function f() {
	return (g + 42)|0;
    }
    return { f:f };
}

{
    assertEq(isAsmJSModule(m_add_global_literal), true);
    let { f } = m_add_global_literal(this, {}, buffer);
    assertEq(f(), 37 + 42);
}

function m_add_global_global(stdlib, ffi, heap) {
    "use asm";

    var g = 37;
    var h = 42;

    function f() {
	return (g + h)|0;
    }
    return { f:f };
}

{
    assertEq(isAsmJSModule(m_add_global_global), true);
    let { f } = m_add_global_global(this, {}, buffer);
    assertEq(f(), 37 + 42);
}

// ============================================================

function m_sub_literals(stdlib, ffi, heap) {
    "use asm";

    function f() {
	return (37 - 42)|0;
    }
    return { f:f };
}

{
    assertEq(isAsmJSModule(m_sub_literals), true);
    let { f } = m_sub_literals(this, {}, buffer);
    assertEq(f(), 37 - 42);
}

function m_sub_global_literal(stdlib, ffi, heap) {
    "use asm";

    var g = 37;

    function f() {
	return (g - 42)|0;
    }
    return { f:f };
}

{
    assertEq(isAsmJSModule(m_sub_global_literal), true);
    let { f } = m_sub_global_literal(this, {}, buffer);
    assertEq(f(), 37 - 42);
}

function m_sub_global_global(stdlib, ffi, heap) {
    "use asm";

    var g = 37;
    var h = 42;

    function f() {
	return (g - h)|0;
    }
    return { f:f };
}

{
    assertEq(isAsmJSModule(m_sub_global_global), true);
    let { f } = m_sub_global_global(this, {}, buffer);
    assertEq(f(), 37 - 42);
}

// ============================================================

// TODO: mul, div, mod


// ============================================================

function m_negInt32(stdlib, ffi, heap) {
    "use asm";

    var v = 10;

    function f() {
	return (-v)|0;
    }
    return { f: f }
}

{
    assertEq(isAsmJSModule(m_negInt32), true);
    let { f } = m_negInt32(this, {}, buffer);
    assertEq(f(), -10);
}

// ============================================================

// TODO: abs
// TODO: clz

// ============================================================

function m_add_literals_d(stdlib, ffi, heap) {
    "use asm";

    function f() {
	return +(12.1 + 7.4);
    }
    return { f:f };
}

{
    assertEq(isAsmJSModule(m_add_literals_d), true);
    let { f } = m_add_literals_d(this, {}, buffer);
    assertEq(f(), 12.1 + 7.4);
}

function m_add_global_literal_d(stdlib, ffi, heap) {
    "use asm";

    var g = 37.5;

    function f() {
	return +(g + 42.8);
    }
    return { f:f };
}

{
    assertEq(isAsmJSModule(m_add_global_literal_d), true);
    let { f } = m_add_global_literal_d(this, {}, buffer);
    assertEq(f(), 37.5 + 42.8);
}

function m_add_global_global_d(stdlib, ffi, heap) {
    "use asm";

    var g = 37.5;
    var h = 42.8;

    function f() {
	return +(g + h);
    }
    return { f:f };
}

{
    assertEq(isAsmJSModule(m_add_global_global_d), true);
    let { f } = m_add_global_global_d(this, {}, buffer);
    assertEq(f(), 37.5 + 42.8);
}

function m_add_global_global_f(stdlib, ffi, heap) {
    "use asm";

    var F = stdlib.Math.fround;
    var g = F(37.5);
    var h = F(42.25);

    function f() {
	return F(g + h);
    }
    return { f:f };
}

{
    assertEq(isAsmJSModule(m_add_global_global_f), true);
    let { f } = m_add_global_global_f(this, {}, buffer);
    assertEq(f(), 37.5 + 42.25);
}

// ============================================================

function m_sub_literals_d(stdlib, ffi, heap) {
    "use asm";

    function f() {
	return +(12.1 - 7.4);
    }
    return { f:f };
}

{
    assertEq(isAsmJSModule(m_sub_literals_d), true);
    let { f } = m_sub_literals_d(this, {}, buffer);
    assertEq(f(), 12.1 - 7.4);
}

function m_sub_global_literal_d(stdlib, ffi, heap) {
    "use asm";

    var g = 37.5;

    function f() {
	return +(g - 42.8);
    }
    return { f:f };
}

{
    assertEq(isAsmJSModule(m_sub_global_literal_d), true);
    let { f } = m_sub_global_literal_d(this, {}, buffer);
    assertEq(f(), 37.5 - 42.8);
}

function m_sub_global_global_d(stdlib, ffi, heap) {
    "use asm";

    var g = 37.5;
    var h = 42.8;

    function f() {
	return +(g - h);
    }
    return { f:f };
}

{
    assertEq(isAsmJSModule(m_sub_global_global_d), true);
    let { f } = m_sub_global_global_d(this, {}, buffer);
    assertEq(f(), 37.5 - 42.8);
}

function m_sub_global_global_f(stdlib, ffi, heap) {
    "use asm";

    var F = stdlib.Math.fround;
    var g = F(37.5);
    var h = F(42.25);

    function f() {
	return F(g - h);
    }
    return { f:f };
}

{
    assertEq(isAsmJSModule(m_sub_global_global_f), true);
    let { f } = m_sub_global_global_f(this, {}, buffer);
    assertEq(f(), 37.5 - 42.25);
}

// ============================================================

function m_neg_d(stdlib, ffi, heap) {
    "use asm";

    var v = 10.8;

    function f() {
	return +(-v);
    }
    return { f: f }
}

{
    assertEq(isAsmJSModule(m_neg_d), true);
    let { f } = m_neg_d(this, {}, buffer);
    assertEq(f(), -10.8);
}

// ============================================================

function m_neg_f(stdlib, ffi, heap) {
    "use asm";

    var F = stdlib.Math.fround;
    var v = F(10.75);

    function f() {
	return F(-v);
    }
    return { f: f }
}

{
    assertEq(isAsmJSModule(m_neg_f), true);
    let { f } = m_neg_f(this, {}, buffer);
    assertEq(f(), -10.75);
}

