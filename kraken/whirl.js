// A whirl is a deformation with a center C and a rotation R.  At C,
// pixels are rotated by R; as we get further from C, the rotation of
// each pixel decreases and the image stretches, and there's a need
// for some interpolation / smoothing.

// That is, the value of an output pixel is not so easy to predict,
// instead we whirl the input image into the output and then fix up
// the output.  The whirl does not parallelize well with striping; we
// can stripe the input but the output grids are not simple stripes.

// Assume for simplicity the image is square with an odd side length L
// and that C is in the center of the image.  Let r=(L-1)/2 and let d
// be the distance of a pixel from C.  Let the rotation o at d be
// R*((d/r)^(1/e)), ie, exponentially smaller as we move away from C.
// Outside the circle defined by C and r, pixels will remain in place.

// In "reality" there would be some drag on all pixels, also outside
// the circle and also if the image is not square.

// The image has Int32 values for our purposes.
// The rotation is expressed in radians.

function whirl(height, width, rotation, dataIn, dataOut) {
    var r = Math.floor((Math.min(width,height)-1)/2);
    var Cx = Math.floor((width-1)/2);
    var Cy = Math.floor((height-1)/2);

    for ( var iny=0 ; iny < height ; iny++ ) {
	for ( var inx=0 ; inx < width ; inx++ ) {
	    var outy = 0;
	    var outx = 0;
	    var distance = Math.sqrt((iny-Cy)*(iny-Cy) + (inx-Cx)*(inx-Cx));
	    if (distance > r) {
		outy = iny;
		outx = inx;
	    }
	    else {
		var alpha = Math.atan2(iny-Cy, inx-Cx);
		var R = distort(rotation, distance, r);
		// clamp calls are probably not necessary
		outy = clamp(0,Cy+Math.floor(Math.sin(alpha + R)*distance),height-1);
		outx = clamp(0,Cx+Math.floor(Math.cos(alpha + R)*distance),width-1);
	    }
	    dataOut[outy*width + outx] = dataIn[iny*width + inx];
	}
    }

    // Compute the missing pixels.

    for ( var outy=0 ; outy < height ; outy++ ) {
	for ( var outx=0 ; outx < width ; outx++ ) {
	    var v = dataOut[outy*width+outx];
	    if (v != 0)
		continue;
	    // For now: pick an adjacent value when possible.  This tends to lead
	    // to streaking, it's better to average.
	    if (outx > 0 && outy > 0)
		dataOut[outy*width+outx] = dataOut[(outy-1)*width+(outx-1)];
	    else if (outy > 0)
		dataOut[outy*width+outx] = dataOut[(outy-1)*width+outx];
	    else if (outx > 0)
		dataOut[outy*width+outx] = dataOut[outy*width+(outx-1)];
	}
    }
}

// Given a nominal rotation and a distance and the radius, return a
// new, possibly smaller, rotation; it should shrink as the distance
// increases toward r.

function distort(rotation, distance, r) {
    // Some kind of exponential would do better.
    return rotation * (r-distance)/r;
}

function clamp(lower, x, upper) {
    if (x < lower) return lower;
    if (x > upper) return upper;
    return x;
}

function whirlTest() {
    var results = document.getElementById('whirl-result');
    results.innerHTML = "Running test...";

    window.setTimeout(function() {
	var canvas = document.getElementById('canvas');
	var ctx = canvas.getContext('2d');

	var img = document.getElementById('image')
	canvas.width = img.width;
	canvas.height = img.height;
	ctx.drawImage(img, 0, 0, img.width, img.height);

	var imgData = ctx.getImageData(0, 0, canvas.width, canvas.height);
	var width = imgData.width;
	var height = imgData.height;
	var dataIn = imgData.data;

	var startTime = Date.now();
	var dataOut = new Uint8Array(dataIn.length);
	whirl(height, width, Math.PI, new Int32Array(dataIn.buffer), new Int32Array(dataOut.buffer));
	var finishTime = Date.now() - startTime;

        for (var i = 0; i < dataOut.length; i++)
            imgData.data[i] = dataOut[i];

	ctx.putImageData(imgData, 0, 0);
	results.innerHTML = "Finished: " + finishTime + "ms";
    }, 10);
}
