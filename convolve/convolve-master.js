// Convolution benchmark running on a single thread in the master, or
// distributed across workers, with source tiles being transfered by
// reference or by copy.
//
// On my 4x2 MacBook Pro with 4 workers and a 4x4 grid, on a 600x640
// image, I get 2-3x speedup over just running the computation on the
// master (it's quite variable).
// 
// Transfering the image bits by reference make a small difference
// only, from about 4.5 to 4.3 ms/iteration.  It could be noise, but
// it seems not to be.  I have verified that transfering works.
//
// There are several ways to write this...  For now, the image is
// divided up once, but the tiles are transfered on every iteration.
// The obvious alternative is to transfer pieces, or perhaps the whole
// image, before all the iterations.  Neither is necessarily
// "realistic".

"use strict";

var USE_WORKERS = true;         // Distribute computation across web workers?
var XFER = true;                // Transfer ownership of tiles back and forth to avoid copying?

const TEST = false;	        // Internal

const NUM_WORKERS = 4;          // Number of workers
const HTILES = 4;               // Number of work tiles in the y direction
const WTILES = 4;               // Number of work tiles in the x direction
const BENCHMARK = true;         // Set to true to run as a benchmark

function ITER() {
    if (TEST)
	return 2;
    if (BENCHMARK)
	return USE_WORKERS ? 200 : 100;
    return 1;
}

/// UI

function onFilesPicked(files, canvas, check_parallel, check_xfer) {
    USE_WORKERS = check_parallel.checked;
    XFER = check_xfer.checked;
    var reader = new FileReader();
    reader.onload = 
        function (e) { 
            processPGM(new Uint8Array(e.target.result), canvas); 
        };
    reader.readAsArrayBuffer(files[0]);
}

function processPGM(bytes, canvas) {
    if (USE_WORKERS) 
        processPGMOnWorkers(bytes, canvas);
    else
        processPGMOnMaster(bytes, canvas);
}

/// Processing-on-worker code

/// Worker pool abstraction

const workers = [];

function lazyCreateWorkers() {
    if (workers.length == 0) 
        for ( var i=0 ; i < NUM_WORKERS ; i++ )
            workers.push(new Worker("convolve-slave.js"));
}

/// Create HTILES*WTILES tiles of the input and distribute the tiles
/// across the available workers.

function processPGMOnWorkers(bytes, canvas) {
    var { loc, width, height, maxval } = readPgm(bytes);
    var outBytes = copyAndZeroPgm(bytes, loc);
    var tiles = [];             // Task queue: [h w height width bytes xfer? id]
    var xfers = [];             // Task queue: [bytes]
    var nextTile = 0;           // Task queue pointer
    var iter = ITER();
    var start, end;             // Time recording
    var numToSend;              // Remaining pieces to send
    var numToReceive;		// Remaining pieces to receive

    function makeGatherPiece(worker) {
        return function (event) {
            if (!event.data)
                return;
            var [h, w, tile, backatcha, tileid] = event.data;
	    numToReceive--;
            if (XFER) {
                tiles[tileid][4] = backatcha;
                xfers[tileid][0] = backatcha;
                tile = new Uint8Array(tile);
            }
            copyTileToResult(outBytes, loc, tile, h, w, height, width);
            if (numToSend > 0) {
		numToSend--;
		numToReceive++;
		var thisTile = nextTile++;
                XFER ? worker.postMessage(tiles[thisTile], xfers[thisTile]) : worker.postMessage(tiles[thisTile]);
            }
	    else if (numToReceive == 0) {
		if (BENCHMARK) {
                    if (--iter == 0) {
			end = Date.now();
			displayPGM(outBytes, canvas);
			alert((end - start)/ITER() + "ms/iteration" + (TEST ? ("\n" + tiles.length + " " + xfers.length) : ""));
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
            XFER ? workers[i].postMessage(tiles[nextTile], xfers[nextTile]) : workers[i].postMessage(tiles[nextTile]);
            nextTile++;
            numToSend--;
	    numToReceive++;
        }
    }

    lazyCreateWorkers();

    for ( var i=0 ; i < workers.length ; i++ )
        workers[i].onmessage = makeGatherPiece(workers[i]);

    for ( var h=0 ; h < HTILES ; h++ )
        for ( var w=0 ; w < WTILES ; w++ ) {
            var [htile, wtile, tile] = copyTileFromSource(bytes, loc, h, w, height, width);
            tiles.push([h, w, htile, wtile, XFER ? tile.buffer : tile, XFER, tiles.length]);
            xfers.push(XFER ? [tile.buffer] : null);
        }

    start = Date.now();
    pump();
}

function copyTileFromSource(bytes, loc, h, w, height, width) {
    var [upper, left, lower, right] = bounds2d(height-2, width-2, HTILES, WTILES, h, w);
    var rows = (lower-upper+3);
    var cols = (right-left+3);
    var out = new Uint8Array(rows*cols);
    copyBytes2d(bytes, loc, height, width, upper-1, left-1, lower+1, right+1, 
                out, 0, lower-upper+3, right-left+3, 0, 0);
    return [rows, cols, out];
}

function copyTileToResult(outBytes, loc, tile, h, w, height, width) {
    var [upper, left, lower, right] = bounds2d(height-2, width-2, HTILES, WTILES, h, w);
    copyBytes2d(tile, 0, lower-upper+3, right-left+3, 1, 1, 1+lower-upper, 1+right-left,
                outBytes, loc, height, width, upper+1, left+1);
}

// Processing-on-master code

function processPGMOnMaster(bytes, canvas) {
    var { loc, width, height, maxval } = readPgm(bytes);
    var outBytes = copyAndZeroPgm(bytes, loc);
    var then = Date.now();
    for ( var i=0 ; i < ITER() ; i++ )
        edgeDetect1(bytes, outBytes, loc, height, width);
    var now = Date.now();
    displayPGM(outBytes, canvas);
    alert(((now - then)/ITER()) + " ms/iter");
}

function edgeDetect1(input, output, loc, height, width) {
    function c1(xmm,xzm,xpm,xmz,xzz,xpz,xmp,xzp,xpp) {
        // (-1  0  1)
        // (-1  0  1)
        // (-1  0  1)
        return -xmm + -xzm + -xpm + xmp + xzp + xpp;
    }
    function c2(xmm,xzm,xpm,xmz,xzz,xpz,xmp,xzp,xpp) {
        // ( 1  1  1)
        // ( 0  0  0)
        // (-1 -1 -1)
        return xmm + xmz + xmp + -xpm + -xpz + -xpp;
    }
    function c3(xmm,xzm,xpm,xmz,xzz,xpz,xmp,xzp,xpp) {
        // (-1 -1 -1)
        // (-1  8 -1)
        // (-1 -1 -1)
        return -xmm + -xzm + -xpm + -xmz + 8*xzz + -xpz + -xmp + -xzp + -xpp;
    }
    function c4(xmm,xzm,xpm,xmz,xzz,xpz,xmp,xzp,xpp) {
        // ( 0  1  0)
        // (-1  0  1)
        // ( 0 -1  0)
        return xmz + -xzm + xzp + -xpz;
    }
    // A hand-written Math.max() is necessary for the parallel versions,
    // but in this serial code the in-lined version cuts the benchmark's
    // running time in half.  (Compare this to cat-convolve-map-outer.js,
    // where Math.max() is faster than the hand-written version.)
    function max2(a,b) { return a > b ? a : b }
    function max4(a,b,c,d) { return max2(max2(a,b),max2(c,d)); }
    function max5(a,b,c,d,e) { return max2(max4(a,b,c,d),e); }
    for ( var h=1 ; h < height-1 ; h++ ) {
        for ( var w=1 ; w < width-1 ; w++ ) {
            var xmm=input[loc+(h-1)*width+(w-1)];
            var xzm=input[loc+h*width+(w-1)];
            var xpm=input[loc+(h+1)*width+(w-1)];
            var xmz=input[loc+(h-1)*width+w];
            var xzz=input[loc+h*width+w];
            var xpz=input[loc+(h+1)*width+w];
            var xmp=input[loc+(h-1)*width+(w+1)];
            var xzp=input[loc+h*width+(w+1)];
            var xpp=input[loc+(h+1)*width+(w+1)];
            var sum=max5(0,
                         c1(xmm,xzm,xpm,xmz,xzz,xpz,xmp,xzp,xpp),
                         c2(xmm,xzm,xpm,xmz,xzz,xpz,xmp,xzp,xpp),
                         c3(xmm,xzm,xpm,xmz,xzz,xpz,xmp,xzp,xpp),
                         c4(xmm,xzm,xpm,xmz,xzz,xpz,xmp,xzp,xpp));
            output[loc+h*width+w] = sum;
        }
    }
    return output;
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
    var out = new Uint8Array(bytes.length);
    for ( var i=0 ; i < loc ; i++ )
        out[i] = bytes[i];
    return out;
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

