/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

var buffer = new ArrayBuffer(65536);

// Parameter passing

// ============================================================

function m_param(stdlib, ffi, heap) {
    "use asm";

    function f(x) {
    	x = x|0;
    	x = (x + 1)|0;
    	return x|0;
    }
    function g(a,b,c,d,e,f,g,h,i,j) { // Some will go on the stack (on x64 at least)
	a = a|0;
	b = b|0;
	c = c|0;
	d = d|0;
	e = e|0;
	f = f|0;
	g = g|0;
	h = h|0;
	i = i|0;
	j = j|0;
	return ((((((((((((((((((a + b)|0) + c)|0) + d)|0) + e)|0) + f)|0) + g)|0) + h)|0) + i)|0) + j)|0);
    }
    function h(y) {
	y = +y;
	return +(y+y);
    }
    function h2(y, z) {		// double, int -> double
	y = +y;
	z = z|0;
	return z|0;
    }
    function h3(y, z) {		// double, int -> int
	y = +y;
	z = z|0;
	return +y;
    }
    return { f:f, g:g, h:h, h2:h2, h3:h3 };
}

{
    assertEq(isAsmJSModule(m_param), true);
    let { f, g, h, h2, h3 } = m_param(this, {}, buffer);
    assertEq(f(33), 34);
    assertEq(g(1,2,4,8,16,32,64,128,256,512), 1023);
    assertEq(h(1.25), 2.5);
    assertEq(h2(1.25, 4), 4);
    assertEq(h3(1.25, 4), 1.25);
}

function m_param_d(stdlib, ffi, heap) {
    "use asm";

    function g(a,b,c,d,e,f,g,h,i,j) { // Some will go on the stack (on x64 at least)
	a = +a;
	b = +b;
	c = +c;
	d = +d;
	e = +e;
	f = +f;
	g = +g;
	h = +h;
	i = +i;
	j = +j;
	return +(+(+(+(+(+(+(+(+(a + b) + c) + d) + e) + f) + g) + h) + i) + j);
    }

    return { g:g };
}

{
    assertEq(isAsmJSModule(m_param_d), true);
    let { g } = m_param_d(this, {}, buffer);
    assertEq(g(1,2,4,8,16,32,64,128,256,512), 1023);
}
