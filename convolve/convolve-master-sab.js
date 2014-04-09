// Convolution benchmark running distributed across workers, using
// SharedArrayBuffers to share input and output data.

"use strict";

const TEST = false;	        // Internal

const NUM_WORKERS = 4;          // Number of workers
const HTILES = 4;               // Number of work tiles in the y direction
const WTILES = 4;               // Number of work tiles in the x direction
const BENCHMARK = true;         // Set to true to run as a benchmark

function ITER() {
    if (TEST)
	return 2;
    if (BENCHMARK)
	return 200;
    return 1;
}

/// UI

function onFilesPicked(files, canvas) {
    var reader = new FileReader();
    reader.onload = 
        function (e) {
	    var data = e.target.result;
	    var inbuf = new Uint8Array(data);
	    var sab = new SharedArrayBuffer(roundup4K(data.byteLength));
	    var buf = new Uint8Array(sab);
	    for ( var i=0, limit=data.byteLength ; i < limit ; i++ )
		buf[i] = inbuf[i];
            processPGMOnWorkers(buf, canvas); 
        };
    reader.readAsArrayBuffer(files[0]);
}

/// Processing-on-worker code

/// Worker pool abstraction

const workers = [];

function lazyCreateWorkers() {
    if (workers.length == 0) 
        for ( var i=0 ; i < NUM_WORKERS ; i++ )
            workers.push(new Worker("convolve-slave-sab.js"));
}

/// Create HTILES*WTILES tiles of the input and distribute the tiles
/// across the available workers.

function processPGMOnWorkers(inBytes, canvas) {
    var { loc, width, height, maxval } = readPgm(inBytes);
    var outBytes = copyAndZeroPgm(inBytes, loc);
    var tiles = [];             // Task queue
    var nextTile = 0;           // Task queue pointer
    var iter = ITER();
    var start, end;             // Time recording
    var numToSend;              // Remaining pieces to send
    var numToReceive;		// Remaining pieces to receive

    function makeGatherPiece(worker) {
        return function (event) {
            var id = event.data;
	    numToReceive--;
            if (numToSend > 0) {
		numToSend--;
		numToReceive++;
                worker.postMessage(tiles[nextTile++]);
            }
	    else if (numToReceive == 0) {
		if (BENCHMARK) {
                    if (--iter == 0) {
			end = Date.now();
			displayPGM(outBytes, canvas);
			alert((end - start)/ITER() + "ms/iteration");
                    }
                    else
			pump();
		}
		else {
                    end = Date.now();
                    displayPGM(outBytes, canvas);
                    alert((end - start)/ITER() + "ms/iteration");
		}
	    }
        }
    }

    function pump() {
        nextTile = 0;
        numToSend = HTILES*WTILES;
	numToReceive = 0;
        for ( var i=0 ; i < workers.length ; i++ ) {
	    workers[i].postMessage(["setup", inBytes.buffer, outBytes.buffer, loc, height, width],
				   [inBytes.buffer, outBytes.buffer]);
	    workers[i].postMessage(tiles[nextTile++]);
            numToSend--;
	    numToReceive++;
	    // It is a significant improvement to keep an extra item in the pipe for the worker
	    // so that more work is available immediately, not when we've roundtripped to the
	    // master.  (The others would also benefit from this.)
	    /*
	    workers[i].postMessage(tiles[nextTile++]);
            numToSend--;
	    numToReceive++;
	    */
        }
    }

    lazyCreateWorkers();

    for ( var i=0 ; i < workers.length ; i++ )
        workers[i].onmessage = makeGatherPiece(workers[i]);

    var id=0;
    for ( var h=0 ; h < HTILES ; h++ ) {
        for ( var w=0 ; w < WTILES ; w++ ) {
	    var [upper, left, lower, right] = bounds2d(height, width, HTILES, WTILES, h, w);
	    // The workers do not work on the internal borders, but they have to be processed too.
	    var uadj = upper > 0 ? 2 : 0;
	    var ladj = left > 0 ? 2 : 0;
	    tiles.push(["work", id++, upper-uadj, left-ladj, lower-upper+1+uadj, right-left+1+ladj]);
        }
    }

    start = Date.now();
    pump();
}

/// Displaying

function displayPGM(bytes, canvas) {
    var { loc, width, height, maxval } = readPgm(bytes);
    var ctx = canvas.getContext('2d');
    canvas.width = width;
    canvas.height = height;
    for ( var h=0 ; h < height ; h++ )
        for ( var w=0 ; w < width ; w++ ) {
            var v = bytes[loc + h*width + w];
            ctx.fillStyle = "rgb(" + v + "," + v + "," + v + ")";
            ctx.fillRect(w, h, w+1, h+1);
        }
}

/// Parsing

function readPgm(bytes) {
    var loc = 0;
    var { loc, word } = getAscii(bytes, loc, true);
    if (word != "P5")
        throw "Bad magic: " + word;
    var { loc, word } = getAscii(bytes, loc);
    var width = parseInt(word);
    var { loc, word } = getAscii(bytes, loc);
    var height = parseInt(word);
    var { loc, word } = getAscii(bytes, loc);
    var maxval = parseInt(word);
    loc++;
    return { loc: loc, width: width, height: height, maxval: maxval };
}

function getAscii(bytes, loc, here) {
    if (!here)
        loc = skipWhite(bytes, loc);
    var s = "";
    while (loc < bytes.length && bytes[loc] > 32)
        s += String.fromCharCode(bytes[loc++]);
    return { loc: loc, word: s };
}

function skipWhite(bytes, loc) {
    while (loc < bytes.length && bytes[loc] <= 32)
        loc++;
    return loc;
}

// Utilities

function copyAndZeroPgm(bytes, loc) {
    var sab = new SharedArrayBuffer(roundup4K(bytes.length));
    var out = new Uint8Array(sab);
    for ( var i=0 ; i < loc ; i++ )
        out[i] = bytes[i];
    return out;
}

function roundup4K(n) {
    return (n + 4095) & ~4095;
}

// Given a grid of size heigh * width, a number of tiles in each
// dimension, and a h,w zero-based tile index, return a vector 
// [upper, left, lower, right] of inclusive coordinates within 
// the grid.

function bounds2d(height, width, htiles, wtiles, h, w) {
    var hsize = (height/htiles)|0;
    var wsize = (width/wtiles)|0;
    var upper = hsize*h;
    var left = wsize*w;
    var lower = h == htiles-1 ? height-1 : upper+hsize-1;
    var right = w == wtiles-1 ? width-1 : left+wsize-1;
    return [upper, left, lower, right];
}

// Given a source array src, and a base offset within it s_loc, and its true dimensions s_height * s_width, 
// and a rectangle within it [s_upper, s_left, s_lower, s_right], copy the rectangle into the
// destination array dest at base offset d_loc, given its true dimensions d_height*d_width, starting
// at location within it [d_upper, d_left].

function copyBytes2d(src, s_loc, s_height, s_width, s_upper, s_left, s_lower, s_right, 
                     dest, d_loc, d_height, d_width, d_upper, d_left)
{
    // s_height and d_height currently unused but could be used for clipping
    var rows = s_lower - s_upper + 1;
    var cols = s_right - s_left + 1;
    for ( var y=0 ; y < rows ; y++ )
        for ( var x=0 ; x < cols ; x++ )
            dest[d_loc + (y + d_upper)*d_width + (x + d_left)] = src[s_loc + (y + s_upper)*s_width + (x + s_left)];
}

