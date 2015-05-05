/* Sudoku solver - potential demo program for shared-memory parallelism */
/* lhansen@mozilla.com / 2015-05-05 */

/* This is a little more evolved than sudoku.js - it avoids some work
 * during constraint propagation, and it tries to be smarter about the
 * order it does things in.
 */

/* Basic rules:
 * A board is 9x9 "slots" with 3x3 "cells" overlaid.
 * When empty cells have been filled in:
 *   Each board row must hold the values 1..9.
 *   Each board column must hold the values 1..9.
 *   Each cell must hold the values 1..9.
 */

var DISPLAY = true;
var ITERATIONS = 10;		// if DISPLAY == false

// How to parallelize?

// Obvious:
//  - clone the board, start the search elsewhere (eg, search slots in a different order)
//  - have a worklist of board positions ***in shared memory***, clone and repeat
//  -

// An easy game, 94 moves with the depth-first strategy.
// http://www.websudoku.com/?level=1&set_id=4350327818
var easy_input =
 [[[0, 6, 7], [0, 5, 9], [8, 0, 0]],
  [[8, 0, 4], [0, 0, 0], [0, 0, 0]],
  [[9, 5, 2], [0, 0, 4], [0, 0, 1]],

  [[0, 7, 9], [0, 0, 5], [0, 0, 3]],
  [[0, 0, 0], [7, 0, 3], [0, 0, 0]],
  [[4, 0, 0], [9, 0, 0], [7, 8, 0]],

  [[6, 0, 0], [5, 0, 0], [1, 9, 2]],
  [[0, 0, 0], [0, 0, 0], [5, 0, 6]],
  [[0, 0, 5], [2, 1, 0], [4, 7, 0]]];

// A hard game, 492 moves with the depth-first strategy.
// http://www.websudoku.com/?level=3&set_id=8111614663
var hard_input =
 [[[0, 0, 0], [0, 0, 5], [0, 9, 0]],
  [[0, 0, 6], [0, 3, 0], [0, 0, 1]],
  [[0, 5, 9], [7, 0, 0], [0, 3, 8]],

  [[0, 4, 0], [0, 0, 8], [0, 0, 0]],
  [[0, 0, 7], [0, 5, 0], [9, 0, 0]],
  [[0, 0, 0], [4, 0, 0], [0, 6, 0]],

  [[7, 9, 0], [0, 0, 3], [4, 1, 0]],
  [[4, 0, 0], [0, 7, 0], [2, 0, 0]],
  [[0, 2, 0], [8, 0, 0], [0, 0, 0]]];

// An evil game, 9333 moves with the depth-first strategy.
// http://www.websudoku.com/?level=4&set_id=2394158150
// Running on node.js 0.10, on an AMD FX-4100, this is solved
// in about 0.19s real time.
//
// However, with sorting, the number of probes increases to 36008.
var evil_input =
 [[[0, 0, 6], [5, 3, 0], [0, 0, 0]],
  [[1, 0, 0], [0, 0, 0], [0, 4, 0]],
  [[0, 0, 8], [0, 0, 9], [0, 3, 0]],

  [[0, 0, 0], [2, 0, 0], [1, 7, 0]],
  [[5, 0, 0], [0, 7, 0], [0, 0, 4]],
  [[0, 7, 4], [0, 0, 3], [0, 0, 0]],

  [[0, 1, 0], [8, 0, 0], [3, 0, 0]],
  [[0, 8, 0], [0, 0, 0], [0, 0, 9]],
  [[0, 0, 0], [0, 1, 6], [2, 0, 0]]];

// This is a little more interesting:
// http://staffhome.ecm.uwa.edu.au/~00013890/sudoku17
//
// The page contains a list of known "minimal unique"
// sudoku games, with 17 clues (64 open spaces) and one solution.
// Below is the first of them.
//
// On the AMD FX-4100 / nodejs 0.10 this requires 6665577 probes
// proceeding in depth-first order without strategy, and takes about
// 97 seconds to complete.
//
// However, with sorting, the number of probes drops to 5699.
var seventeen_1 =
    [[[0,0,0],[0,0,0],[0,1,0],],
     [[4,0,0],[0,0,0],[0,0,0],],
     [[0,2,0],[0,0,0],[0,0,0],],
     [[0,0,0],[0,5,0],[4,0,7],],
     [[0,0,8],[0,0,0],[3,0,0],],
     [[0,0,1],[0,9,0],[0,0,0],],
     [[3,0,0],[4,0,0],[2,0,0],],
     [[0,5,0],[1,0,0],[0,0,0],],
     [[0,0,0],[8,0,6],[0,0,0],]];

var GAME = seventeen_1;

//////////////////////////////////////////////////////////////////////
//
// Code below this point.

var probes = 0;

// SpiderMonkey shell polyfill.  This pisses off node.js for some reason.
/*
if (!this.console)
    console = { log: print };
*/

run(GAME);

// A game state comprises a list of open slots and an Uint16Array of
// length 81.  The array contains the board in row-major order.
//
// An unfilled array slot with candidate numbers X, Y, Z has bits
// (1<<X), (1<<Y), and (1<<Z) set.  If all numbers are candidates then
// the value is 0x3FE.
//
// A filled slot has the value (1<<15) | (X<<10).  The low bits
// (except the lowest bit) are garbage and should be ignored.
//
// The lowest bit is currently never set.
//
// The search proceeds by picking an empty slot, picking one of its
// possible values, and then propagating that value along the row,
// column, and in the cell of the slot, removing it from the candidate
// sets of the conflicting empty slots.  If a candidate set thus
// becomes empty the search backtracks: that placement is not possible
// with that game state.

function run(input) {
    var _x = setup(input);
    var b = _x[0];
    var empty = _x[1];
    console.log("Number of open spaces: " + empty.length);
    for ( var i=0 ; i < (DISPLAY ? 1 : ITERATIONS) ; i++ )
	search(b, empty, 0);
}

function setup(input_board) {
    var vals = flatten(input_board);
    var b = new Uint16Array(81);
    var empty = [];

    // Initial layout.
    for ( var r=0 ; r < 9 ; r++ ) {
	for ( var c=0 ; c < 9 ; c++ ) {
	    var x = r*9+c;
	    if (!vals[x]) {
		b[x] = 0x3FE;	// All values available
		empty.push([r,c]);
	    }
	    else
		b[x] = 0x8000 | (vals[x] << 10);
	}
    }

    // Constrain the slots based on values.
    for ( var i=0 ; i < vals.length ; i++ )
	if (vals[i] != 0)
	    constrain(b, Math.floor(i / 9), i % 9, vals[i]);

    // Simple strategy: go from more information (fewer bits set)
    // toward less.  This has interesting effects... on the evil
    // input, it quadruples the number of probes.  But on the
    // seventeen_1 input, it drops the number of probes to 5699, a
    // reduction by a factor of 1169 (!).
    empty.sort(function (v, w) { return bitsSet(b[v[0]*9+v[1]]) - bitsSet(b[w[0]*9+w[1]]) });

    return [b, empty];
}

// Since the board is small we just copy it and update the copy,
// rather than tracking the cells that were changed and their old
// values.
//
// (If storage use is a concern then empty arrays can be cached in a
// global list, obviously they obey stack discipline.)

function search(b, empty, k) {
    if (k >= empty.length) {
	if (DISPLAY)
	    done(b);
	return true;
    }
    probes++;
    var _x = empty[k];
    var r = _x[0];
    var c = _x[1];
    var x = r*9+c;
    var as = b[x];
    var b2 = new Uint16Array(81);
    for ( var i=(1<<1), j=1 ; i <= (1<<9) ; i<<=1, j++ ) {
	if (as & i) {
	    b2.set(b);
	    b2[x] = 0x8000 | (j << 10);
	    if (constrain(b2, r, c, j)) {
		if (search(b2, empty, k+1))
		    return true;
	    }
	}
    }
    return false;
}

// The value 'val' is set in [r,c], so remove it from all cells in row
// r, column c, and all slots in the cell containing [r,c].

function constrain(b, row, col, val) {
    var mask = ~(1 << val);
    var ok = true;
    for ( var c=0 ; c < 9 ; c++ ) {
	var x=row*9+c;
	if (!(b[x] &= mask))
	    if (c != col) ok = false;
    }
    for ( var r=0 ; r < 9 ; r++ ) {
	var x=r*9+col;
	if (!(b[x] &= mask))
	    if (r != row) ok = false;
    }
    var cbase = Math.floor(col / 3)*3;
    var rbase = Math.floor(row / 3)*3;
    for ( var r=0 ; r < 3 ; r++ ) {
	for ( var c = 0 ; c < 3 ; c++ ) {
	    var x=(rbase+r)*9 + (cbase+c);
	    if (!(b[x] &= mask))
		if ((rbase+r) != row && (cbase+c) != col) ok = false;
	}
    }
    return ok;
}

// Utility code below this point

function done(b) {
    console.log("Probes=" + probes);
    printBoard(b, false);
    check(b);
}

function check(b) {
    for ( var r=0 ; r < 9 ; r++ ) {
	var m = 0;
	for ( var c=0 ; c < 9 ; c++ )
	    m |= checkVal(b, r, c);
	if (m != 0x3FE)
	    throw "Wrong value at row " + r;
    }
    for ( var c=0 ; c < 9 ; c++ ) {
	var m = 0;
	for ( var r=0 ; r < 9 ; r++ )
	    m |= checkVal(b, r, c);
	if (m != 0x3FE)
	    throw "Wrong value at column " + c;
    }
    for ( var y=0 ; y < 3 ; y++ )
	for ( var x=0 ; x < 3 ; x++ ) {
	    var m = 0;
	    for ( var r=0 ; r < 3 ; r++ )
		for ( var c = 0 ; c < 3 ; c++ )
		    m |= checkVal(b, (y*3)+r, (x*3)+c);
	    if (m != 0x3FE)
		throw "Wrong value in cell " + y + "," + x;
	}
}

// Value should have high bit set and the payload X should be 1..9.
// Return (1<<X).

function checkVal(b, r, c) {
    var v = b[r*9+c];
    var bad = "";
    if (!(v & 0x8000))
	bad += "1";
    v = (v >> 10) & 15;
    if (v < 1 || v > 9)
	bad += "2";
    if (bad)
	throw "Bad value at " + r + "," + c + ": " + saved.toString(16) + " -- " + bad + " " + j;
    return (1 << v);
}

function printBoard(b, strict) {
    for ( var r=0 ; r < 9 ; r++ ) {
	if (r && !(r % 3))
	    console.log("");
	var s = "";
	for ( var c=0; c < 9 ; c++ ) {
	    if (c && !(c % 3))
		s += "  ";
	    var x = r*9+c;
	    if (strict && !(b[x] & 0x8000))
		throw "Bad cell: [" + r + " " + c + "] = " + b[x].toString(16);
	    var v = "";
	    if (!(b[x] & 0x8000))
		v = "[" + b[x].toString(16) + "]";
	    else
		v = ((b[x] >> 10) & 15).toString();
	    if (!strict)
		s += "      ".substring(v.length) + v;
	}
	console.log(s);
    }
}

function bitsSet(x) {
    var n = 0;
    while (x != 0) {
	if (x & 1)
	    n++;
	x >>>= 1;
    }
    return n;
}

function flatten(a) {
    var r = [];
    function flatten_(a) {
	if (!(a instanceof Array)) {
	    r.push(a);
	    return;
	}
	for ( var i=0 ; i < a.length ; i++ )
	    flatten_(a[i]);
    }
    flatten_(a);
    return r;
}
