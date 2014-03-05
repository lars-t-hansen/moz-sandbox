// Simple convolution benchmark (serial version)
// 2014-02-28 / lhansen@mozilla.com
//
// For testing, run like this:
//
//  js cat-convolve.js | ./unhex > out.pgm
//
// For benchmarking, set 'benchmark' to true and run like this:
//
//  js cat-convolve.js
//
// With 'typedobj' set to false, this uses Uint8Array.
// With 'typedobj' set to true, it uses TypedObject.ArrayType(TypedObject.uint8)

const benchmark = true;
const typedobj = true;
const iterations = benchmark ? 100 : 1;

const T = TypedObject;
const ByteArrayType = new T.ArrayType(T.uint8);

const { loc, bytes, height, width, maxval } = readPgm("cat.pgm");
if (maxval > 255)
    throw "Bad maxval: " + maxval;

// For testing encoding, etc: just flip the image across the x axis in-place
//putstr(encode(flip(bytes, loc, height, width)));

// Actual work: Convolving the image
var out = copyAndZeroPgm(bytes, loc);
var r = time(
    function () {
	var r;
	for ( var i=0 ; i < iterations ; i++ ) {
	    r = null;
	    r = edgeDetect1(bytes, out, loc, height, width);
	}
	return r;
    });
		 
if (benchmark)
    print(r.time);
else
    putstr(encode(r.result));

quit();

// Flip image in-place across x-axis

function flip(bytes, loc, height, width) {
    for ( var h=0 ; h < height/2 ; h++ ) {
	for ( var w=0 ; w < width ; w++ ) {
	    var a = loc+(h*width)+w;
	    var b = loc+(height-h)*width+w;
	    var t = bytes[a];
	    bytes[a] = bytes[b];
	    bytes[b] = t;
	}
    }
    return bytes;
}

// http://blancosilva.wordpress.com/teaching/mathematical-imaging/edge-detection-the-convolution-approach/
// Faler's approach

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
	    var sum=Math.max(c1(xmm,xzm,xpm,xmz,xzz,xpz,xmp,xzp,xpp),
			     c2(xmm,xzm,xpm,xmz,xzz,xpz,xmp,xzp,xpp),
			     c3(xmm,xzm,xpm,xmz,xzz,xpz,xmp,xzp,xpp),
			     c4(xmm,xzm,xpm,xmz,xzz,xpz,xmp,xzp,xpp));
	    output[loc+h*width+w] = sum;
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
    var bytes;
    if (typedobj) {
	bytes = new (ByteArrayType.dimension(_bytes.length));
	for ( var i=0 ; i < _bytes.length ; i++ )
	    bytes[i] = _bytes[i];
    }
    else
	bytes = _bytes;
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
    var out = typedobj ? new (ByteArrayType.dimension(bytes.length)) : new Uint8Array(bytes.length);
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
