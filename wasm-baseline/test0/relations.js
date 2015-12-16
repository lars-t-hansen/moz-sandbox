/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

var buffer = new ArrayBuffer(65536);

// The asm-to-wasm translator performs no constant folding, so the
// test (literal OP literal) actually test code generation for OP.

// ============================================================

function m_eq_literals(stdlib, ffi, heap) {
    "use asm";

    function f() {
	return (37 == 42)|0;
    }
    function g() {
	return (37 == 37)|0;
    }
    return { f:f, g:g };
}

{
    assertEq(isAsmJSModule(m_eq_literals), true);
    let { f, g } = m_eq_literals(this, {}, buffer);
    assertEq(f(), 0);
    assertEq(g(), 1);
}

function m_eq_global_literal(stdlib, ffi, heap) {
    "use asm";

    var v = 37;

    function f() {
	return ((v|0) == 42)|0;
    }
    function g() {
	return ((v|0) == 37)|0;
    }
    return { f:f, g:g };
}

{
    assertEq(isAsmJSModule(m_eq_global_literal), true);
    let { f, g } = m_eq_global_literal(this, {}, buffer);
    assertEq(f(), 0);
    assertEq(g(), 1);
}

function m_eq_global_global(stdlib, ffi, heap) {
    "use asm";

    var v = 37;
    var w = 42;
    var x = 37;

    function f() {
	return ((v|0) == (w|0))|0;
    }
    function g() {
	return ((v|0) == (x|0))|0;
    }
    return { f:f, g:g };
}

{
    assertEq(isAsmJSModule(m_eq_global_global), true);
    let { f, g } = m_eq_global_global(this, {}, buffer);
    assertEq(f(), 0);
    assertEq(g(), 1);
}

// ============================================================

function m_ne_literals(stdlib, ffi, heap) {
    "use asm";

    function f() {
	return (37 != 42)|0;
    }
    function g() {
	return (37 != 37)|0;
    }
    return { f:f, g:g };
}

{
    assertEq(isAsmJSModule(m_ne_literals), true);
    let { f, g } = m_ne_literals(this, {}, buffer);
    assertEq(f(), 1);
    assertEq(g(), 0);
}

function m_ne_global_literal(stdlib, ffi, heap) {
    "use asm";

    var v = 37;

    function f() {
	return ((v|0) != 42)|0;
    }
    function g() {
	return ((v|0) != 37)|0;
    }
    return { f:f, g:g };
}

{
    assertEq(isAsmJSModule(m_ne_global_literal), true);
    let { f, g } = m_ne_global_literal(this, {}, buffer);
    assertEq(f(), 1);
    assertEq(g(), 0);
}

function m_ne_global_global(stdlib, ffi, heap) {
    "use asm";

    var v = 37;
    var w = 42;
    var x = 37;

    function f() {
	return ((v|0) != (w|0))|0;
    }
    function g() {
	return ((v|0) != (x|0))|0;
    }
    return { f:f, g:g };
}

{
    assertEq(isAsmJSModule(m_ne_global_global), true);
    let { f, g } = m_ne_global_global(this, {}, buffer);
    assertEq(f(), 1);
    assertEq(g(), 0);
}

// ============================================================

function m_lt_literals(stdlib, ffi, heap) {
    "use asm";

    function f() {
	return (37 < 42)|0;
    }
    function g() {
	return (37 < 37)|0;
    }
    return { f:f, g:g };
}

{
    assertEq(isAsmJSModule(m_lt_literals), true);
    let { f, g } = m_lt_literals(this, {}, buffer);
    assertEq(f(), 1);
    assertEq(g(), 0);
}

function m_lt_global_literal(stdlib, ffi, heap) {
    "use asm";

    var v = 37;

    function f() {
	return ((v|0) < 42)|0;
    }
    function g() {
	return ((v|0) < 37)|0;
    }
    return { f:f, g:g };
}

{
    assertEq(isAsmJSModule(m_lt_global_literal), true);
    let { f, g } = m_lt_global_literal(this, {}, buffer);
    assertEq(f(), 1);
    assertEq(g(), 0);
}

function m_lt_global_global(stdlib, ffi, heap) {
    "use asm";

    var v = 37;
    var w = 42;
    var x = 37;

    function f() {
	return ((v|0) < (w|0))|0;
    }
    function g() {
	return ((v|0) < (x|0))|0;
    }
    return { f:f, g:g };
}

{
    assertEq(isAsmJSModule(m_lt_global_global), true);
    let { f, g } = m_lt_global_global(this, {}, buffer);
    assertEq(f(), 1);
    assertEq(g(), 0);
}

// ============================================================

function m_le_literals(stdlib, ffi, heap) {
    "use asm";

    function f() {
	return (37 <= 42)|0;
    }
    function g() {
	return (37 <= 37)|0;
    }
    function h() {
	return (42 <= 37)|0;
    }
    return { f:f, g:g, h:h };
}

{
    assertEq(isAsmJSModule(m_le_literals), true);
    let { f, g, h } = m_le_literals(this, {}, buffer);
    assertEq(f(), 1);
    assertEq(g(), 1);
    assertEq(h(), 0);
}

function m_le_global_literal(stdlib, ffi, heap) {
    "use asm";

    var v = 37;

    function f() {
	return ((v|0) <= 42)|0;
    }
    function g() {
	return ((v|0) <= 37)|0;
    }
    function h() {
	return (42 <= (v|0))|0;
    }
    return { f:f, g:g, h:h };
}

{
    assertEq(isAsmJSModule(m_le_global_literal), true);
    let { f, g, h } = m_le_global_literal(this, {}, buffer);
    assertEq(f(), 1);
    assertEq(g(), 1);
    assertEq(h(), 0);
}

function m_le_global_global(stdlib, ffi, heap) {
    "use asm";

    var v = 37;
    var w = 42;
    var x = 37;

    function f() {
	return ((v|0) <= (w|0))|0;
    }
    function g() {
	return ((v|0) <= (x|0))|0;
    }
    function h() {
	return ((w|0) <= (v|0))|0;
    }
    return { f:f, g:g, h:h };
}

{
    assertEq(isAsmJSModule(m_le_global_global), true);
    let { f, g, h } = m_le_global_global(this, {}, buffer);
    assertEq(f(), 1);
    assertEq(g(), 1);
    assertEq(h(), 0);
}

// ============================================================

function m_gt_literals(stdlib, ffi, heap) {
    "use asm";

    function f() {
	return (37 > 42)|0;
    }
    function g() {
	return (37 > 37)|0;
    }
    function h() {
	return (42 > 37)|0;
    }
    return { f:f, g:g, h:h };
}

{
    assertEq(isAsmJSModule(m_gt_literals), true);
    let { f, g, h } = m_gt_literals(this, {}, buffer);
    assertEq(f(), 0);
    assertEq(g(), 0);
    assertEq(h(), 1);
}

function m_gt_global_literal(stdlib, ffi, heap) {
    "use asm";

    var v = 37;

    function f() {
	return ((v|0) > 42)|0;
    }
    function g() {
	return ((v|0) > 37)|0;
    }
    function h() {
	return (42 > (v|0))|0;
    }
    return { f:f, g:g, h:h };
}

{
    assertEq(isAsmJSModule(m_gt_global_literal), true);
    let { f, g, h } = m_gt_global_literal(this, {}, buffer);
    assertEq(f(), 0);
    assertEq(g(), 0);
    assertEq(h(), 1);
}

function m_gt_global_global(stdlib, ffi, heap) {
    "use asm";

    var v = 37;
    var w = 42;
    var x = 37;

    function f() {
	return ((v|0) > (w|0))|0;
    }
    function g() {
	return ((v|0) > (x|0))|0;
    }
    function h() {
	return ((w|0) > (v|0))|0;
    }
    return { f:f, g:g, h:h };
}

{
    assertEq(isAsmJSModule(m_gt_global_global), true);
    let { f, g, h } = m_gt_global_global(this, {}, buffer);
    assertEq(f(), 0);
    assertEq(g(), 0);
    assertEq(h(), 1);
}

// ============================================================

function m_ge_literals(stdlib, ffi, heap) {
    "use asm";

    function f() {
	return (37 >= 42)|0;
    }
    function g() {
	return (37 >= 37)|0;
    }
    return { f:f, g:g };
}

{
    assertEq(isAsmJSModule(m_ge_literals), true);
    let { f, g } = m_ge_literals(this, {}, buffer);
    assertEq(f(), 0);
    assertEq(g(), 1);
}

function m_ge_global_literal(stdlib, ffi, heap) {
    "use asm";

    var v = 37;

    function f() {
	return ((v|0) >= 42)|0;
    }
    function g() {
	return ((v|0) >= 37)|0;
    }
    return { f:f, g:g };
}

{
    assertEq(isAsmJSModule(m_ge_global_literal), true);
    let { f, g } = m_ge_global_literal(this, {}, buffer);
    assertEq(f(), 0);
    assertEq(g(), 1);
}

function m_ge_global_global(stdlib, ffi, heap) {
    "use asm";

    var v = 37;
    var w = 42;
    var x = 37;

    function f() {
	return ((v|0) >= (w|0))|0;
    }
    function g() {
	return ((v|0) >= (x|0))|0;
    }
    return { f:f, g:g };
}

{
    assertEq(isAsmJSModule(m_ge_global_global), true);
    let { f, g } = m_ge_global_global(this, {}, buffer);
    assertEq(f(), 0);
    assertEq(g(), 1);
}

