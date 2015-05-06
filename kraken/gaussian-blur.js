/* The original program is buggy, as the blur of pixels to the right
   of and below previously processed pixels will incorporate the
   blurred values, not the original pixel values (input array ==
   output array).  This program fixes that bug by having a separate
   output array.

   There is some other cleanup here, but the program logic has not
   otherwise been altered.
*/

"use strict";

setup();

function blurTest() {
    var results = document.getElementById('blur-result');
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
	computeBlur(0, height, height, width, dataIn, dataOut);
	var finishTime = Date.now() - startTime;

        for (var i = 0; i < dataOut.length; i++)
            imgData.data[i] = dataOut[i];

	ctx.putImageData(imgData, 0, 0);
	results.innerHTML = "Finished: " + finishTime + "ms (kernelSize=" + kernelSize_ + ")";
    }, 10);
}
