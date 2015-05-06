/* This implements the blur in parallel by means of shared memory.
   */

"use strict";

const workers = [];
const numWorkers = 8;

setup();

for ( var i=0 ; i < numWorkers ; i++ ) {
    var w = new Worker("gaussian-blur-shared-worker.js");
    w.postMessage(["start"]);	// Warmup
    workers.push(w);
}

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
	var data = imgData.data;

	// It's only fair that scatter/gather time is included in the time.
	var startTime = Date.now();

	var remaining = numWorkers;

	var shmemIn = new SharedUint8Array(data.length);
	var shmemOut = new SharedUint8Array(data.length);

	function allDone() {
	    var finishTime = Date.now() - startTime;

	    for ( var i=0 ; i < data.length ; i++ )
		data[i] = shmemOut[i];

	    ctx.putImageData(imgData, 0, 0);
	    results.innerHTML = "Finished: " + finishTime + "ms";
	};

	for ( var i=0 ; i < data.length ; i++ )
	    shmemIn[i] = data[i];

	for ( var id=0 ; id < numWorkers ; id++ ) {
	    var w = workers[id];
	    w.onmessage =
		function (ev) {
		    console.log("Done " + ev.data[0]);
		    if (--remaining == 0)
			allDone();
		};

	    var sliceHeight = Math.floor(height / workers.length)
	    var row_first = sliceHeight * id;
	    var row_lim = id == numWorkers-1 ? height : sliceHeight * (id + 1);

	    w.postMessage(["do", shmemIn.buffer, shmemOut.buffer, id, row_first, row_lim, height, width],
			  [shmemIn.buffer, shmemOut.buffer]);
	}

    }, 10);
}
