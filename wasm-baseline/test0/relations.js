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

function m_eq_global_global_d(stdlib, ffi, heap) {
    "use asm";

    var v = 37.5;
    var w = 42.8;
    var x = 37.5;

    function f() {
	return (v == w)|0;
    }
    function g() {
	return (v == x)|0;
    }
    return { f:f, g:g };
}

{
    assertEq(isAsmJSModule(m_eq_global_global_d), true);
    let { f, g } = m_eq_global_global_d(this, {}, buffer);
    assertEq(f(), 0);
    assertEq(g(), 1);
}

function m_eq_global_global_f(stdlib, ffi, heap) {
    "use asm";

    var F = stdlib.Math.fround;
    var v = F(37.5);
    var w = F(42.75);
    var x = F(37.5);

    function f() {
	return (v == w)|0;
    }
    function g() {
	return (v == x)|0;
    }
    return { f:f, g:g };
}

{
    assertEq(isAsmJSModule(m_eq_global_global_f), true);
    let { f, g } = m_eq_global_global_f(this, {}, buffer);
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

function m_ne_global_global_d(stdlib, ffi, heap) {
    "use asm";

    var v = 37.5;
    var w = 42.8;
    var x = 37.5;

    function f() {
	return (v != w)|0;
    }
    function g() {
	return (v != x)|0;
    }
    return { f:f, g:g };
}

{
    assertEq(isAsmJSModule(m_ne_global_global_d), true);
    let { f, g } = m_ne_global_global_d(this, {}, buffer);
    assertEq(f(), 1);
    assertEq(g(), 0);
}

function m_ne_global_global_f(stdlib, ffi, heap) {
    "use asm";

    var F = stdlib.Math.fround;
    var v = F(37.5);
    var w = F(42.75);
    var x = F(37.5);

    function f() {
	return (v != w)|0;
    }
    function g() {
	return (v != x)|0;
    }
    return { f:f, g:g };
}

{
    assertEq(isAsmJSModule(m_ne_global_global_f), true);
    let { f, g } = m_ne_global_global_f(this, {}, buffer);
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

function m_lt_global_global_d(stdlib, ffi, heap) {
    "use asm";

    var v = 37.5;
    var w = 42.8;
    var x = 37.5;

    function f() {
	return (v < w)|0;
    }
    function g() {
	return (v < x)|0;
    }
    return { f:f, g:g };
}

{
    assertEq(isAsmJSModule(m_lt_global_global_d), true);
    let { f, g } = m_lt_global_global_d(this, {}, buffer);
    assertEq(f(), 1);
    assertEq(g(), 0);
}

function m_lt_global_global_f(stdlib, ffi, heap) {
    "use asm";

    var F = stdlib.Math.fround;
    var v = F(37.5);
    var w = F(42.75);
    var x = F(37.5);

    function f() {
	return (v < w)|0;
    }
    function g() {
	return (v < x)|0;
    }
    return { f:f, g:g };
}

{
    assertEq(isAsmJSModule(m_lt_global_global_f), true);
    let { f, g } = m_lt_global_global_f(this, {}, buffer);
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

function m_le_global_global_d(stdlib, ffi, heap) {
    "use asm";

    var v = 37.5;
    var w = 42.8;
    var x = 37.5;

    function f() {
	return (v <= w)|0;
    }
    function g() {
	return (v <= x)|0;
    }
    function h() {
	return (w <= v)|0;
    }
    return { f:f, g:g, h:h };
}

{
    assertEq(isAsmJSModule(m_le_global_global_d), true);
    let { f, g, h } = m_le_global_global_d(this, {}, buffer);
    assertEq(f(), 1);
    assertEq(g(), 1);
    assertEq(h(), 0);
}

function m_le_global_global_f(stdlib, ffi, heap) {
    "use asm";

    var F = stdlib.Math.fround;
    var v = F(37.5);
    var w = F(42.75);
    var x = F(37.5);

    function f() {
	return (v <= w)|0;
    }
    function g() {
	return (v <= x)|0;
    }
    function h() {
	return (w <= v)|0;
    }
    return { f:f, g:g, h:h };
}

{
    assertEq(isAsmJSModule(m_le_global_global_f), true);
    let { f, g, h } = m_le_global_global_f(this, {}, buffer);
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

function m_gt_global_global_d(stdlib, ffi, heap) {
    "use asm";

    var v = 37.5;
    var w = 42.8;
    var x = 37.5;

    function f() {
	return (v > w)|0;
    }
    function g() {
	return (v > x)|0;
    }
    function h() {
	return (w > v)|0;
    }
    return { f:f, g:g, h:h };
}

{
    assertEq(isAsmJSModule(m_gt_global_global_d), true);
    let { f, g, h } = m_gt_global_global_d(this, {}, buffer);
    assertEq(f(), 0);
    assertEq(g(), 0);
    assertEq(h(), 1);
}

function m_gt_global_global_f(stdlib, ffi, heap) {
    "use asm";

    var F = stdlib.Math.fround;
    var v = F(37.5);
    var w = F(42.75);
    var x = F(37.5);

    function f() {
	return (v > w)|0;
    }
    function g() {
	return (v > x)|0;
    }
    function h() {
	return (w > v)|0;
    }
    return { f:f, g:g, h:h };
}

{
    assertEq(isAsmJSModule(m_gt_global_global_f), true);
    let { f, g, h } = m_gt_global_global_f(this, {}, buffer);
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

function m_ge_global_global_d(stdlib, ffi, heap) {
    "use asm";

    var v = 37.5;
    var w = 42.8;
    var x = 37.5;

    function f() {
	return (v >= w)|0;
    }
    function g() {
	return (v >= x)|0;
    }
    return { f:f, g:g };
}

{
    assertEq(isAsmJSModule(m_ge_global_global_d), true);
    let { f, g } = m_ge_global_global_d(this, {}, buffer);
    assertEq(f(), 0);
    assertEq(g(), 1);
}

function m_ge_global_global_f(stdlib, ffi, heap) {
    "use asm";

    var F = stdlib.Math.fround;
    var v = F(37.5);
    var w = F(42.75);
    var x = F(37.5);

    function f() {
	return (v >= w)|0;
    }
    function g() {
	return (v >= x)|0;
    }
    return { f:f, g:g };
}

{
    assertEq(isAsmJSModule(m_ge_global_global_f), true);
    let { f, g } = m_ge_global_global_f(this, {}, buffer);
    assertEq(f(), 0);
    assertEq(g(), 1);
}


// ============================================================

function m_not(stdlib, ffi, heap) {
    "use asm";

    var v = 10;
    var w = 0;

    function f() {
	return (!v)|0;
    }
    function g() {
	return (!w)|0;
    }
    function h() {
	return (!!v)|0;
    }
    return { f:f, g:g, h:h }
}

{
    assertEq(isAsmJSModule(m_not), true);
    let { f, g, h } = m_not(this, {}, buffer);
    assertEq(f(), 0);
    assertEq(g(), 1);
    assertEq(h(), 1);
}

// ============================================================

function m_conditional(stdlib, ffi, heap) {
    "use asm";

    var v = 10;
    var w = 0;
    var x = 37;
    var y = 42;

    function f() {
	return (v ? x : y)|0;
    }
    function g() {
	return (w ? x : y)|0;
    }
    return { f:f, g:g }
}

{
    assertEq(isAsmJSModule(m_conditional), true);
    let { f, g } = m_conditional(this, {}, buffer);
    assertEq(f(), 37);
    assertEq(g(), 42);
}

function m_conditional_d(stdlib, ffi, heap) {
    "use asm";

    var v = 10;
    var w = 0;
    var x = 37.5;
    var y = 42.5;

    function f() {
	return +(v ? x : y);
    }
    function g() {
	return +(w ? x : y);
    }
    return { f:f, g:g }
}

{
    assertEq(isAsmJSModule(m_conditional_d), true);
    let { f, g } = m_conditional_d(this, {}, buffer);
    assertEq(f(), 37.5);
    assertEq(g(), 42.5);
}

function m_conditional_f(stdlib, ffi, heap) {
    "use asm";

    var F = stdlib.Math.fround;
    var v = 10;
    var w = 0;
    var x = F(37.5);
    var y = F(42.5);

    function f() {
	return F(v ? x : y);
    }
    function g() {
	return F(w ? x : y);
    }
    return { f:f, g:g }
}

{
    assertEq(isAsmJSModule(m_conditional_f), true);
    let { f, g } = m_conditional_f(this, {}, buffer);
    assertEq(f(), 37.5);
    assertEq(g(), 42.5);
}
