/* Some experiments with asm.js */

function testmod(stdlib, ffi/*, heap*/) {
    "use asm";

    const _newObj = ffi.newObj;
    const _moveReg = ffi.moveReg;
    const _clearReg = ffi.clearReg;
    const _isNullReg = ffi.isNullReg;
    const _newReg = ffi.newReg;
    const _disposeReg = ffi.disposeReg;
    const _pushRegs = ffi.pushRegs;
    const _popRegs = ffi.popRegs;
    const _getFieldVal = ffi.getFieldVal;
    const _getFieldReg = ffi.getFieldReg;
    const _setFieldVal = ffi.setFieldVal;
    const _setFieldReg = ffi.setFieldReg;
    const _print = ffi.print;

    /* Convention: when a function returns a Reg, the destination reg
       is passed as the first argument; the function fills it in.
       */
    /* FIXME: the primitives should also have destination as 1st arg */

    /* Integer lists */

    function cons(/*Reg*/rtn, /*val*/a, /*Reg*/b) {
	rtn = rtn|0;
	a = a|0;
	b = b|0;
	var p = 0;
	p = _pushRegs(1)|0;	// rtn and b may alias, cf Fortran
	_newObj(p|0);
	_setFieldVal(p|0, 0, a|0);
	_setFieldReg(p|0, 1, b|0);
	_moveReg(p|0, rtn|0);
	_popRegs(1);
    }

    function car(/*Reg*/obj) /*val*/ {
	obj = obj|0;
	return _getFieldVal(obj|0, 0)|0;
    }

    function cdr(/*Reg*/rtn, /*Reg*/obj) {
	rtn = rtn|0;
	obj = obj|0;
	_getFieldReg(obj|0, 1, rtn|0);
    }

    function list(/*Reg*/rtn, /*val*/a, /*val*/b, /*val*/c) {
	rtn = rtn|0;
	a = a|0;
	b = b|0;
	c = c|0;
	cons(rtn, c, 0);
	cons(rtn, b, rtn);
	cons(rtn, a, rtn);
    }

    // print(list(1,2,3))
    function test() {
	var p = 0;
	p = _pushRegs(1)|0;
	list(p, 1, 2, 3);
	while (!(_isNullReg(p|0)|0)) {
	    _print(car(p)|0);
	    cdr(p, p);
	}
	_popRegs(1);
    }

    return { test: test }
}

// Object management.
//
// The story is this:
//
// - a Reg is represented as an integer (this is a necessary reveal)
// - a Reg is opaque but holds an object reference
// - there is an operation for allocating and deallocating stack Regs
// - there is an operation for allocating and deallocating global Regs
// - there is an operation for copying a Reg to another
// - there is an operation for clearing a Reg (without deallocating it)
// - there is an operation for testing whether a Reg is null
// - there is an operation for allocating an object and storing it in a Reg
// - Reg 0 always holds a null value
// - there is a return Reg used for transfering a reference from a callee to a caller
// - operations on objects are operations on a Reg that holds the object
// - property names on objects are nonnegative integers, for now
//
// Behind the scenes:
//
// - if the integer is positive it represents a Reg on the shadow stack
// - if the integer is negative it represents a global Reg

// Internals

var stackRegs = [null];		// 1, 2, ...
var globalRegs = {};
var nextGlob = 1;		// -1, -2, ...

function setReg(reg, v) {
    if (reg < 0)
	globalRegs[-reg] = v;
    else
	stackRegs[reg] = v;
}

function getReg(reg) {
    return reg < 0 ? globalRegs[-reg] : stackRegs[reg];
}

// Public

function newObj(reg) {
    setReg(reg, {});
}

function moveReg(src, dest) {
    setReg(dest, getReg(src));
}

function clearReg(reg) {
    setReg(reg, null);
}

function isNullReg(reg) {
    return getReg(reg) === null ? 1 : 0;
}

function newReg() {
    var r = nextGlob++;
    globalRegs[r] = null;
    return -r;
}

function disposeReg(reg) {
    delete globalRegs[-r];
}

function pushRegs(n) {
    var p = stackRegs.length;
    for ( var i=0 ; i < n ; i++ )
	stackRegs.push(null);
    return p;
}

function popRegs(n) {
    stackRegs.length -= n;
}

function getFieldVal(ro, field) {
    return getReg(ro)[field|0];
}

function getFieldReg(ro, field, rd) {
    setReg(rd, getReg(ro)[field|0]);
}

function setFieldVal(ro, field, val) {
    getReg(ro)[field|0] = val;
}

function setFieldReg(ro, field, rs) {
    getReg(ro)[field|0] = getReg(rs);
}

var ffi = {
    newObj: newObj,
    moveReg: moveReg,
    clearReg: clearReg,
    isNullReg: isNullReg,
    newReg: newReg,
    disposeReg: disposeReg,
    pushRegs: pushRegs,
    popRegs: popRegs,
    getFieldVal: getFieldVal,
    getFieldReg: getFieldReg,
    setFieldVal: setFieldVal,
    setFieldReg: setFieldReg,
    print: print
};

var exports = testmod(this, ffi);
exports.test();

