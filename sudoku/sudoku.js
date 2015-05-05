/* Sudoku solver - potential demo program for shared-memory parallelism? */
/* lhansen@mozilla.com / 2015-05-05 */

/* Basic rules:
 * A board is 9x9 "slots" with 3x3 "cells" overlaid.
 * When empty cells have been filled in:
 *   Each board row must hold the values 1..9.
 *   Each board column must hold the values 1..9.
 *   Each cell must hold the values 1..9.
 */

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

var probes = 0;

run(easy_input);

// A game state comprises a list of open slots and an Uint16Array of
// length 81.  The array contains the board in row-major order.
//
// An unfilled array slot with candidate numbers X, Y, Z has bits
// (1<<X), (1<<Y), and (1<<Z) set.
//
// A filled slot with value X has bit (1<<X) set, along with (1<<15).
//
// The search proceeds by picking an empty slot, picking one of its
// possible values, and then propagating that value along the row,
// column, and in the cell of the slot, removing it from the candidate
// sets of the conflicting empty slots.  If a set thus becomes empty
// the search backtracks: that number is not possible with that game
// state.
//
// Here we start with an unordered set of empty slots.  But the
// empties could be sorted by fewest number of candidates first (and
// could in principle be re-sorted as we go along).

function run(input) {
    try {
	var _x = setup(input);
	var b = _x[0];
	var empty = _x[1];
	console.log("Number of open spaces: " + empty.length);
	search(b, empty, 0);
    }
    catch (e) {
	if (e === "Done")
	    return;
	throw e;
    }
}

function setup(input_board) {
    var vals = flatten(input_board);
    var b = new Uint16Array(81);
    var empty = [];

    // Initial layout.
    for ( var r=0 ; r < 9 ; r++ ) {
	for ( var c=0 ; c < 9 ; c++ ) {
	    var x = r*9+c;
	    if (vals[x] == 0) {
		b[x] = 0x3FE;	// All values available
		empty.push([r,c]);
	    }
	    else
		b[x] = 0x8000 | (1 << vals[x]);
	}
    }

    // Constrain the slots based on values.
    for ( var i=0 ; i < vals.length ; i++ )
	if (vals[i] != 0)
	    constrain(b, Math.floor(i / 9), i % 9, vals[i]);

    return [b, empty];
}

// Since the board is small we just copy it and update the copy,
// rather than tracking the cells that were changed and their old
// values.
//
// (If storage use is a concern then empty arrays can be cached in a
// global list.)

function search(b, empty, k) {
    if (k >= empty.length)
	done(b);
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
	    b2[x] = 0x8000 | i;
	    if (constrain(b2, r, c, j))
		search(b2, empty, k+1);
	}
    }
}

// The value 'val' is set in [r,c], so remove it from all cells in row
// r, column c, and all slots in the cell containing [r,c].
//
// We could simplify this by a redundant representation that makes the
// low-order bits don't care if the upper bit is set; the cell value
// 1..9 would fit into the upper bits: there are five bits available.

function constrain(b, row, col, val) {
    var mask = ~(1 << val);
    var ok = true;
    for ( var c=0 ; c < 9 ; c++ ) {
	if (c != col) {
	    var x=row*9+c;
	    if (!(b[x] & 0x8000))
		if (!(b[x] &= mask))
		    ok = false;
	}
    }
    for ( var r=0 ; r < 9 ; r++ ) {
	if (r != row) {
	    var x=r*9+col;
	    if (!(b[x] & 0x8000))
		if (!(b[x] &= mask))
		    ok = false;
	}
    }
    var cbase = Math.floor(col / 3)*3;
    var rbase = Math.floor(row / 3)*3;
    for ( var r=0 ; r < 3 ; r++ ) {
	for ( var c = 0 ; c < 3 ; c++ ) {
	    if ((rbase+r) != row && (cbase+c) != col) {
		var x=(rbase+r)*9 + (cbase+c);
		if (!(b[x] & 0x8000))
		    if (!(b[x] &= mask))
			ok = false;
	    }
	}
    }
    return ok;
}

// Utility code below this point

function done(b) {
    printBoard(b, false);
    check(b);
    throw "Done";
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

// Value should have high bit set and exactly one of the
// 1..9 bits set.  It returns that bit.
function checkVal(b, r, c) {
    var v = b[r*9+c];
    var bad = "";
    if (!(v & 0x8000))
	bad += "1";
    v &= 0x7FFF;
    if (v & ~0x3FE)
	bad += "2";
    var saved = v;
    var j = 0;
    while (v > 0) {
	if (v & 1)
	    j++;
	v >>= 1;
    }
    if (j != 1)
	bad += "3";
    if (bad)
	throw "Bad value at " + r + "," + c + ": " + saved.toString(16) + " -- " + bad + " " + j;
    return saved;
}

function printBoard(b, strict) {
    console.log("Probes=" + probes);
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
		v = lookup(b[x]).toString();
	    if (!strict)
		s += "      ".substring(v.length) + v;
	}
	console.log(s);
    }
}

function lookup(v) {
    v = (v & 0x7FFF) >> 1;
    var i=0;
    while (v > 0) {
	i++;
	v >>= 1;
    }
    return i;
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
