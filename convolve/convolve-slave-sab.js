var input;			// Uint8Array sitting on a SharedArrayBuffer
var output;			// Uint8Array sitting on a SharedArrayBuffer
var loc;			// Location (0,0) of the grid within input and output
var width;			// Number of columns in input
var height;			// Number of rows in input

onmessage = function (ev) {
    //console.log(ev.data);
    switch (ev.data[0]) {
    case "setup":
	var input_sab, output_sab, dummy;
	[dummy, input_sab, output_sab, loc, height, width] = ev.data;
	input = new Uint8Array(input_sab);
	output = new Uint8Array(output_sab);
	break;
    case "work":
	var [dummy, id, upper, left, rows, cols] = ev.data;
	edgeDetect1(upper, left, rows, cols);
	postMessage(id);
	break;
    }
}

function edgeDetect1(upper, left, rows, cols) {
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
    var width = this.width;
    var loc = this.loc;
    var input = this.input;
    for ( var h=upper+1 ; h < upper+rows-1 ; h++ ) {
	for ( var w=left+1 ; w < left+cols-1 ; w++ ) {
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
