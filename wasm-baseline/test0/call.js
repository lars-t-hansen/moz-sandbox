/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

var buffer = new ArrayBuffer(65536);

function m_call_direct(stdlib, ffi, heap) {
    "use asm";

    function f(x) {
	x = x|0;
	return ((g(x)|0) + (h(x)|0))|0;
    }
    function g(x) {
	x = x|0;
	return (x+10)|0;
    }
    function h(x) {
	x = x|0;
	return (x-5)|0;
    }
    return { f:f };
}

{
    assertEq(isAsmJSModule(m_call_direct), true);
    let { f } = m_call_direct(this, {}, buffer);
    assertEq(f(3), 11);
}

function m_fib(stdlib, ffi, heap) {
    "use asm";

    function f(x) {
	x = x|0;
	if ((x|0) < 2)
	    return x|0;
	return (((f((x-1)|0))|0) + (f((x-2)|0)|0))|0;
    }
    return { f:f };
}

{
    assertEq(isAsmJSModule(m_fib), true);
    let { f } = m_fib(this, {}, buffer);
    assertEq(f(10), 55);
}

function m_fib_d(stdlib, ffi, heap) {
    "use asm";

    function f(x) {
	x = +x;
	if (+x < 2.0)
	    return +x;
	return +(+f(+(x-1.0)) + +f(+(x-2.0)));
    }
    return { f:f };
}

{
    assertEq(isAsmJSModule(m_fib_d), true);
    let { f } = m_fib_d(this, {}, buffer);
    //print(f(10));
    assertEq(f(10), 55);
}

function m_forward_d(stdlib, ffi, heap) {
    "use asm";

    function f(a,b,c,d,e,f,g,h,i,j) {
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
	return +g(a,b,c,d,e,f,g,h,i,j);
    }
    function g(a,b,c,d,e,f,g,h,i,j) {
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

    return { f:f };
}

{
    assertEq(isAsmJSModule(m_forward_d), true);
    let { f } = m_forward_d(this, {}, buffer);
    assertEq(f(1,2,4,8,16,32,64,128,256,512), 1023);
}

function m_forward_mix(stdlib, ffi, heap) {
    "use asm";

    function f(a,b,c,d,e,f,g,h,i,j) {
	a = +a;
	b = b|0;
	c = +c;
	d = d|0;
	e = +e;
	f = f|0;
	g = +g;
	h = h|0;
	i = +i;
	j = j|0;
	return +g(a,b,c,d,e,f,g,h,i,j);
    }
    function g(a,b,c,d,e,f,g,h,i,j) {
	a = +a;
	b = b|0;
	c = +c;
	d = d|0;
	e = +e;
	f = f|0;
	g = +g;
	h = h|0;
	i = +i;
	j = j|0;
	return +(+(+(+(+(+(+(+(+(a + +(b|0)) + c) + +(d|0)) + e) + +(f|0)) + g) + +(h|0)) + i) + +(j|0));
    }

    return { f:f };
}

{
    assertEq(isAsmJSModule(m_forward_mix), true);
    let { f } = m_forward_mix(this, {}, buffer);
    assertEq(f(1,2,4,8,16,32,64,128,256,512), 1023);
}

function m_call_f(stdlib, ffi, heap) {
    "use asm";

    var F = stdlib.Math.fround;

    function f(x) {
	x = F(x);
	return F(g(x));
    }

    function g(x) {  // float -> float
	x = F(x);
	return F(x+x);
    }

    return { f:f };
}

{
    assertEq(isAsmJSModule(m_call_f), true);
    let { f } = m_call_f(this, {}, buffer);
    assertEq(f(1), 2);
}
