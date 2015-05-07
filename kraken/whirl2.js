// A variation on whirl that uses a different idea.

// The idea here is to iterate over the output grid, and to compute pixel
// values from the input grid based on the reverse transformation.  This
// can do better since it can take into account the neighborhood of pixels
// at the source for each target.

// Really the distortion needs to be better: using a circle is not
// quite right.  Consider a point on the edge.  This point is "stuck".
// All points between the center and this point on a straight line in
// the original get to be translated along a curve that connects the
// center with that point.  Furthermore, as that line gets longer, it
// stretches more near the center than near the edge.
//
// For that kind of a distortion we may instead iterate over rays from
// the center to the edge (for every point on the edge); an output
// pixel is affected by every ray that intersects with it, and there's
// some antialiasing going on.
//
// (It may be that an exponential that drops to zero at the edge is
// good enough?)

function whirl(height, width, rotation, dataIn, dataOut) {
    var r = Math.floor((Math.max(width,height)-1)/2);
    var Cx = Math.floor((width-1)/2);
    var Cy = Math.floor((height-1)/2);

    for ( var y=0 ; y < height ; y++ ) {
	for ( var x=0 ; x < width ; x++ ) {
	    var iny = 0;
	    var inx = 0;
	    var distance = Math.sqrt((y-Cy)*(y-Cy) + (x-Cx)*(x-Cx));
	    var alpha = Math.atan2(y-Cy, x-Cx);
	    var R = distort(-rotation, distance, r);
	    iny = clamp(0,Cy+Math.floor(Math.sin(alpha + R)*distance),height-1);
	    inx = clamp(0,Cx+Math.floor(Math.cos(alpha + R)*distance),width-1);
	    // Here we could sample the neighborhood of the source pixel, which would
	    // allow for higher quality in distorted regions.
	    dataOut[y*width + x] = dataIn[iny*width + inx];
	}
    }
}

// Given a nominal rotation and a distance and the radius, return a
// new, possibly smaller, rotation; it should shrink as the distance
// increases toward r.

function distort(rotation, distance, r) {
    // Some kind of exponential might do better.
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
	var imgOut = ctx.createImageData(canvas.width, canvas.height);
	var width = imgData.width;
	var height = imgData.height;
	var dataIn = imgData.data;

	var distortion = 0;
	var delta = Math.PI / 30;

	window.setInterval(function () {
	    var startTime = Date.now();
	    var dataOut = imgOut.data;
	    distortion += delta;
	    if (Math.abs(distortion) > 2*Math.PI)
		delta = -delta;
	    whirl(height, width, distortion, new Int32Array(dataIn.buffer), new Int32Array(dataOut.buffer));
	    var finishTime = Date.now() - startTime;
	    ctx.putImageData(imgOut, 0, 0);
	}, 20);
	//results.innerHTML = "Finished: " + finishTime + "ms";
    }, 10);
}
