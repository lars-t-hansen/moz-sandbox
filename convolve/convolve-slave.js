onmessage = function (ev) {
    if (!ev.data)
	return;
    var [h, w, height, width, bytes, xfer, xferloc] = ev.data;
    input = xfer ? new Uint8Array(bytes) : bytes;
    var output = new Uint8Array(height*width);
    edgeDetect1(input, output, height, width);
    // If the input buffer was transfered then transfer it back so
    // that it can be reused for whatever purpose.
    console.log("Done with piece: " + h + "," + w);
    if (xfer)
	postMessage([h, w, output.buffer, bytes, xferloc], [bytes, output.buffer]);
    else
	postMessage([h, w, output, null, 0]);
}

function edgeDetect1(input, output, height, width) {
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
	    var xmm=input[(h-1)*width+(w-1)];
	    var xzm=input[h*width+(w-1)];
	    var xpm=input[(h+1)*width+(w-1)];
	    var xmz=input[(h-1)*width+w];
	    var xzz=input[h*width+w];
	    var xpz=input[(h+1)*width+w];
	    var xmp=input[(h-1)*width+(w+1)];
	    var xzp=input[h*width+(w+1)];
	    var xpp=input[(h+1)*width+(w+1)];
	    var sum=max5(0,
			 c1(xmm,xzm,xpm,xmz,xzz,xpz,xmp,xzp,xpp),
			 c2(xmm,xzm,xpm,xmz,xzz,xpz,xmp,xzp,xpp),
			 c3(xmm,xzm,xpm,xmz,xzz,xpz,xmp,xzp,xpp),
			 c4(xmm,xzm,xpm,xmz,xzz,xpz,xmp,xzp,xpp));
	    output[h*width+w] = sum;
	}
    }
    return output;
}
