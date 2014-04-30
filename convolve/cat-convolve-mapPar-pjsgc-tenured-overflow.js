// PJS GC test: simple convolution that allocates enough temporary but
// non-evacuated objects per element to kick off a series of garbage
// collections that cause the PJS nursery to grow to its full capacity,
// and still not be large enough to accomodate the live data within
// the standard load factor.  Thus the data structure will be promoted
// on the next minor GC.
//
// Eventually the tenured area will fill up and need to be collected
// but the volume of this test is not such that that is a major factor.
//
// The current (2014-04-28) policy for the nursery is that it is grown
// until its live size is no greater than 1/3 the size of the nursery,
// but not beyond 4MB.  (There's no way to introspect to get those
// parameters.)  Also the policy for triggering evacuation is pretty
// simple: if the nursery was expanded to its max when the minor GC
// starts, and collection leaves it more than 1/3 full, then the next
// collection will be a promotion.  We thus need a 1.5MB data
// structure and enough junk allocation to kick off two collections.
//
// The program also uses the objects in such a way that a GC bug will
// lead to a crash or wrong result.
//
// 2014-04-28 / lhansen@mozilla.com
//
// For testing, run like this:
//
//  js cat-convolve-mapPar-pjsgc-nursery-overflow.js | ./unhex > out.pgm
//
// For stress testing, set 'benchmark' to true and run like this:
//
//  js cat-convolve-mapPar-nursery-overflow.js

const benchmark = false;
const iterations = benchmark ? 100 : 1;

const T = TypedObject;
const IX = new T.ArrayType(T.uint32);

const { loc, bytes, height, width, maxval } = readPgm("cat.pgm");
if (maxval > 255)
    throw "Bad maxval: " + maxval;

// Actual work: Convolving the image
var indices = IX.buildPar(width-2, x => x+1) // [1,2,...,width-2]

// This bootstraps the workers but makes little difference in practice
var warmup = edgeDetect1(bytes, indices, loc, height, width);

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
	    out[loc+(h*width)+w] = r.result[h-1][w-1];
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
	result.push(indices.mapPar(
	    function (w) {
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
		// Hide the result deep in a list, GC will kick in occasionally
		function wabbit(n) {
		    if (n == 0)
			return { value: sum, left: null, right: null };
		    return { value: 0, left: wabbit(n-1), right: wabbit(n-1) };
		}
		// depth n yields 2^(n+1) - 1 nodes
		// one node is perhaps 80 bytes on 32-bit
		// solve:     2^(n+1) * 80 >= 1536*1024
		//        <=> n+1 * lg 2 + lg 80 >= lg 512 + lg 1024
                //        <=> n >= (lg 1536 + lg 1024 - lg 80) - 1
		//        <=> n >= 22 - 6 - 1
		//        <=> n >= 15
		if (h == 100 && w % 10 == 0) {
		    var q = wabbit(15);
		    // Pick the leftmost value in that tree
		    while (q.left != null)
			q = q.left;
		    return q.value;
		}
		else
		    return sum;
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
