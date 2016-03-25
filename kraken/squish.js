// input and output are rgba images, we create output pixels by
// computing averages of the values in the input for a grid
// corresponding to the scale factor.  This is probably a bit crude.

function scaleDown(input, height, width, output, outheight, outwidth, outy_lo, outy_hi, outx_lo, outx_hi) {
    var scaley = height/outheight;
    var scalex = width/outwidth;

    // This is more or less OK for shrinking but it's not good for
    // scaling up or deforming.  For scaling up, a pixel value would be some
    // function of pixels in the area of its source, perhaps an
    // interpolation of adjacent pixels.

    for ( var y=outy_lo ; y <= outy_hi ; y++ ) {
	for ( var x=outx_lo ; x <= outx_hi ; x++ ) {
	    output[y*outwidth*4+x*4+0] = input[Math.floor(y*scaley)*width*4+Math.floor(x*scalex)*4+0];
	    output[y*outwidth*4+x*4+1] = input[Math.floor(y*scaley)*width*4+Math.floor(x*scalex)*4+1];
	    output[y*outwidth*4+x*4+2] = input[Math.floor(y*scaley)*width*4+Math.floor(x*scalex)*4+2];
	    output[y*outwidth*4+x*4+3] = input[Math.floor(y*scaley)*width*4+Math.floor(x*scalex)*4+3];
	}
    }
}

function squishTest() {
    var results = document.getElementById('blur-result');
    results.innerHTML = "Running test...";

    window.setTimeout(function() {
	var canvas = document.getElementById('canvas');
	var ctx = canvas.getContext('2d');

	var scale = 0.2;

	var img = document.getElementById('image')
	var width = img.width;
	var height = img.height;

	canvas.width = width;
	canvas.height = height;
	ctx.drawImage(img, 0, 0, width, height);
	var dataIn = ctx.getImageData(0, 0, width, height).data;

	var s_width = Math.round(width*scale);
	var s_height = Math.round(height*scale);

	var startTime = Date.now();
	var dataOut = new Uint8ClampedArray(s_width*s_height*4)
	scaleDown(dataIn, height, width, dataOut, s_height, s_width, 0, s_height-1, 0, s_width-1);
	var finishTime = Date.now() - startTime;

	canvas.width = s_width;
	canvas.height = s_height;
	var imgData = ctx.getImageData(0, 0, s_width, s_height);

        for (var i = 0; i < dataOut.length; i++)
            imgData.data[i] = dataOut[i];

	ctx.putImageData(imgData, 0, 0);
	results.innerHTML = "Finished: " + finishTime + "ms";
    }, 10);
}
