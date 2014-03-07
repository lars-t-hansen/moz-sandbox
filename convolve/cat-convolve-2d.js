// Simple convolution benchmark (serial version, TypedObject 2d arrays)
// 2014-02-28 / lhansen@mozilla.com
//
// For testing, run like this:
//
//  js cat-convolve-2d.js | ./unhex > out.pgm
//
// For benchmarking, set 'benchmark' to true and run like this:
//
//  js cat-convolve-2d.js

const benchmark = true;
const iterations = benchmark ? 100 : 1;

const { header, grid, height, width, maxval } = readPgm("cat.pgm");
if (maxval > 255)
    throw "Bad maxval: " + maxval;

// For testing encoding, etc: just flip the image across the x axis in-place
//putstr(encode(header, flip(grid)));

// Actual work: Convolving the image
var out = new (TypedObject.uint8.array(width).array(height));
var r = time(
    function () {
	var r;
	for ( var i=0 ; i < iterations ; i++ ) {
	    r = null;
	    r = edgeDetect1(grid, out);
	}
	return r;
    });
if (benchmark)
    print(r.time);
else
    putstr(encode(header, r.result));

quit();

// Flip image in-place across x-axis

function flip(grid) {
    for ( var h=0, height=grid.length ; h < height/2 ; h++ ) {
	for ( var w=0, width=grid[h].length ; w < width ; w++ ) {
	    var t = grid[h][w];
	    grid[h][w] = grid[height-1-h][w];
	    grid[height-1-h][w] = t;
	}
    }
    return grid;
}

// http://blancosilva.wordpress.com/teaching/mathematical-imaging/edge-detection-the-convolution-approach/
// Faler's approach

function edgeDetect1(grid, output) {
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
    function max2(a,b) { return a > b ? a : b }
    function max4(a,b,c,d) { return max2(max2(a,b),max2(c,d)); }
    function max5(a,b,c,d,e) { return max2(max4(a,b,c,d),e); }
    var height=grid.length;
    var width=grid[0].length;
    // A hand-written Math.max is necessary for the parallel versions.
    // In cat-convolve.js, Math.max slows the program down by a factor
    // of two; here by a factor of 15, presumably a bug or some sort of
    // deoptimization.  See bug 978077.
    for ( var h=1 ; h < height-1 ; h++ ) {
	for ( var w=1 ; w < width-1 ; w++ ) {
	    var xmm=grid[h-1][w-1];
	    var xzm=grid[h][w-1];
	    var xpm=grid[h+1][w-1];
	    var xmz=grid[h-1][w];
	    var xzz=grid[h][w];
	    var xpz=grid[h+1][w];
	    var xmp=grid[h-1][w+1];
	    var xzp=grid[h][w+1];
	    var xpp=grid[h+1][w+1];
	    var sum=max5(0,
			 c1(xmm,xzm,xpm,xmz,xzz,xpz,xmp,xzp,xpp),
			 c2(xmm,xzm,xpm,xmz,xzz,xpz,xmp,xzp,xpp),
			 c3(xmm,xzm,xpm,xmz,xzz,xpz,xmp,xzp,xpp),
			 c4(xmm,xzm,xpm,xmz,xzz,xpz,xmp,xzp,xpp));
	    output[h][w] = sum;
	}
    }
    return output;
}

// Benchmarking

function time(thunk) {
    var then = Date.now();
    var r = thunk();
    var now = Date.now();
    return { time:now - then, result:r};
}

// PGM input and output

function readPgm(filename) {
    var _bytes = snarf(filename, "binary"); // Uint8Array
    var loc = 0;
    var { loc, word } = getAscii(_bytes, loc, true);
    if (word != "P5")
	throw "Bad magic: " + word;
    var { loc, word } = getAscii(_bytes, loc);
    var width = parseInt(word);
    var { loc, word } = getAscii(_bytes, loc);
    var height = parseInt(word);
    var { loc, word } = getAscii(_bytes, loc);
    var maxval = parseInt(word);
    loc++;
    var T = TypedObject.uint8.array(width).array(height);
    var grid = new T;
    for ( var h=0 ; h < height ; h++ )
	for ( var w=0 ; w < width ; w++ ) 
	    try { grid[h][w] = _bytes[loc+h*width+w]; } catch (e) { throw height + " " + width + " " + e + " " + h + " " + w }
    return { header: _bytes.subarray(0,loc), grid: grid, width: width, height: height, maxval: maxval };
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

function encode(header, grid) {
    function hex(n) {
	return "0123456789abcdef".charAt(n);
    }
    function put(out, xs) {
	for ( var i=0, limit=xs.length ; i < limit ; i++ ) {
	    var c = xs[i];
	    out += hex((c >> 4) & 15);
	    out += hex(c & 15);
	}
	return out;
    }
    var out = put("",header);
    for ( var h=0 ; h < grid.length ; h++ )
	out = put(out,grid[h]);
    return out;
}
