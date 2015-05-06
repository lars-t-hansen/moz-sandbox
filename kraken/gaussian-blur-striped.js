/* This implements the blur in parallel by means of striping and
   memory transfer.  A horizontal stripe of each image, which overlaps
   with adjacent stripes, is copied into a buffer that is transfered
   to each worker as the input memory.  Each worker allocates new
   memory that is shipped back by transfer as output memory and is
   then copied into the output.
   */

"use strict";

const workers = [];
const numWorkers = 8;

// We need the kernel length to compute overlaps for the striping
setup();

for ( var i=0 ; i < numWorkers ; i++ ) {
    var w = new Worker("gaussian-blur-striped-worker.js");
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
	var info = new Array(numWorkers);
	var received = new Array(numWorkers);

	function allDone() {
	    var finishTime = Date.now() - startTime;

	    for ( var i=0 ; i < imgData.data.length ; i++ )
		imgData.data[i] = 0;

	    // Need not be done here, can overlap with computation in
	    // message handler.  But that skews the timing for the benchmark.
	    for ( var i=0 ; i < info.length ; i++ ) {
		var dataOut = received[i];
		var [row_first, row_lim, base_y] = info[i];
		console.log(row_first + " " + row_lim + " " + base_y);
		for ( var y=row_first ; y < row_lim ; y++ )
		    for ( var x=0 ; x < width*4 ; x++ )
			imgData.data[y*width*4+x] = dataOut[(base_y+(y-row_first))*width*4+x];
	    }

	    ctx.putImageData(imgData, 0, 0);
	    results.innerHTML = "Finished: " + finishTime + "ms (kernelSize=" + kernelSize_ + ")";
	};

	var id = 0;
	for ( var id=0 ; id < numWorkers ; id++ ) {
	    var w = workers[id];
	    w.onmessage =
		function (ev) {
		    var [mem, id] = ev.data;
		    received[id] = new Uint8Array(mem);
		    if (--remaining == 0)
			allDone();
		};

	    var overlap = kernel_.length - 1; // Loop runs from 1-kernelSize to kernelSize-1
	    var sliceHeight = Math.floor(height / workers.length)
	    var row_first, row_lim, h, copy_first, copy_lim;
	    if (id == 0) {
		row_first = 0;
		row_lim = sliceHeight;
		h = sliceHeight + overlap;
		copy_first = 0;
		copy_lim = h;
	    }
	    else if (id == numWorkers-1) {
		row_first = sliceHeight * (numWorkers-1);
		row_lim = height;
		h = (row_lim - row_first) + overlap;
		copy_first = row_first - overlap;
		copy_lim = height;
	    }
	    else {
		row_first = sliceHeight * id;
		row_lim = sliceHeight * (id + 1);
		h = sliceHeight + 2*overlap;
		copy_first = row_first - overlap;
		copy_lim = row_lim + overlap;
	    }
	    var mem = new Uint8Array(h*width*4);
	    for ( var y=copy_first ; y < copy_lim ; y++ )
		for ( var x=0 ; x < width*4 ; x++ )
		    mem[(y-copy_first)*width*4 + x] = data[y*width*4 + x];
	    info[id] = [row_first, row_lim, (row_first - copy_first)];
	    w.postMessage([mem.buffer, id, h, width], [mem.buffer]);
	}

    }, 10);
}
