// PJS GC test: simple convolution that delivers its results as boxed
// object values, within objects that are allocated directly in the
// tenured area (functions) by means of a pointer to another object
// that is allocated in the nursery.  The latter objects will need 
// to be evacuated from the PJS nurseries at the end of the parallel
// section or we'll crash or get the wrong results, but for that to
// work the root scanning of the tenured area must work.
//
// 2014-04-25 / lhansen@mozilla.com
//
// For testing, run like this:
//
//  js cat-convolve-mapPar-pjsgc-tenured.js | ./unhex > out.pgm
//
// For stress testing, set 'benchmark' to true and run like this:
//
//  js cat-convolve-mapPar-pjsgc-tenured.js

// NOTE: This test must use Array and Object, not TypedObject array
// and struct, because there are currently primitives on TypedObject
// array involving references that cause the PJS engine to bail out.

const benchmark = true;
const iterations = benchmark && scriptArgs[0] != "once" ? 100 : 1;

const { loc, bytes, height, width, maxval } = readPgm("cat.pgm");
if (maxval > 255)
    throw "Bad maxval: " + maxval;

var indices = new Array(width-2);

var r = time(
    function () {
	var r;
	for ( var i=0 ; i < iterations ; i++ ) {
	    r = null;
	    r = edgeDetect1(bytes, indices, loc, height, width);
	}
	return r;
    });
		 
if (benchmark)
    print(r.time);
else {
    // r.result is an Array of TypedObject arrays representing rows 1..height-2 but 
    // with the first and last columns missing.   Slam it into an output array and
    // copy over the original bits for the borders.
    var out = copyAndZeroPgm(bytes, loc);
    for ( var h=1 ; h < height-1 ; h++ )
	for ( var w=1 ; w < width-1 ; w++ )
	    out[loc+(h*width)+w] = (r.result[h-1][w-1])().value;
    // ...
    putstr(encode(out));
}

quit();

// http://blancosilva.wordpress.com/teaching/mathematical-imaging/edge-detection-the-convolution-approach/
// Faler's approach

function edgeDetect1(input, indices, loc, height, width) {
    function c1(xmm,xzm,xpm,xmz,xzz,xpz,xmp,xzp,xpp) {
	// (-1  0  1)
	// (-1  0  1)
	// (-1  0  1)
	return 0 - xmm - xzm - xpm + xmp + xzp + xpp;
    }
    function c2(xmm,xzm,xpm,xmz,xzz,xpz,xmp,xzp,xpp) {
	// ( 1  1  1)
	// ( 0  0  0)
	// (-1 -1 -1)
	return xmm + xmz + xmp - xpm - xpz - xpp;
    }
    function c3(xmm,xzm,xpm,xmz,xzz,xpz,xmp,xzp,xpp) {
	// (-1 -1 -1)
	// (-1  8 -1)
	// (-1 -1 -1)
	return 0 - xmm - xzm - xpm - xmz + 8*xzz - xpz - xmp - xzp - xpp;
    }
    function c4(xmm,xzm,xpm,xmz,xzz,xpz,xmp,xzp,xpp) {
	// ( 0  1  0)
	// (-1  0  1)
	// ( 0 -1  0)
	return xmz - xzm + xzp - xpz;
    }
    function max2(a,b) { return a > b ? a : b }
    function max4(a,b,c,d) { return max2(max2(a,b),max2(c,d)); }
    function max5(a,b,c,d,e) { return max2(max4(a,b,c,d),e); }
    var result = [];
    for ( var h=1 ; h < height-1 ; h++ ) {
	// Map across the prototypical array, ignoring the input value but 
	// using the index
	result.push(indices.mapPar(
	    function (_, idx) {
		var w = idx+1;
		var xmm=input[loc+(h-1)*width+(w-1)];
		var xzm=input[loc+h*width+(w-1)];
		var xpm=input[loc+(h+1)*width+(w-1)];
		var xmz=input[loc+(h-1)*width+w];
		var xzz=input[loc+h*width+w];
		var xpz=input[loc+(h+1)*width+w];
		var xmp=input[loc+(h-1)*width+(w+1)];
		var xzp=input[loc+h*width+(w+1)];
		var xpp=input[loc+(h+1)*width+(w+1)];
		// Math.max not supported in parallel sections (bug 979859)
		var sum=max5(0,
			     c1(xmm,xzm,xpm,xmz,xzz,xpz,xmp,xzp,xpp),
			     c2(xmm,xzm,xpm,xmz,xzz,xpz,xmp,xzp,xpp),
			     c3(xmm,xzm,xpm,xmz,xzz,xpz,xmp,xzp,xpp),
			     c4(xmm,xzm,xpm,xmz,xzz,xpz,xmp,xzp,xpp));
		// Box it, the box will require evacuation at the end of the parallel section
		// fbox will be tenured but will contain, through its scope, a reference to the nursery object valbox
		var valbox = { value: sum };              // nursery object
		var fbox = function () { return valbox }; // tenured object
		return fbox;				  // tenured object will be stored in the result array
	    }));
    }
    return result;
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
    var bytes = snarf(filename, "binary"); // Uint8Array
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
    return { bytes: bytes, loc: loc, width: width, height: height, maxval: maxval };
}

function copyAndZeroPgm(bytes, loc) {
    var out = new Uint8Array(bytes.length);
    for ( var i=0 ; i < loc ; i++ )
	out[i] = bytes[i];
    return out;
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

function encode(xs) {
    function hex(n) {
	return "0123456789abcdef".charAt(n);
    }
    var out = "";
    for ( var i=0, limit=xs.length ; i < limit ; i++ ) {
	var c = xs[i];
	out += hex((c >> 4) & 15);
	out += hex(c & 15);
    }
    return out;
}
