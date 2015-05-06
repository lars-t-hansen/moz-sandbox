importScripts("gaussian-computation.js");

setup();

onmessage =
    function (ev) {
	var [tag, memIn, memOut, id, row_min, row_lim, height, width] = ev.data;
	if (tag == "start")
	    return;
	console.log("Worker " + id + " starting: " + [id, row_min, row_lim, width].join(","));
	var dataIn = new SharedUint8Array(memIn);
	var dataOut = new SharedUint8Array(memOut);
	computeBlur(row_min, row_lim, height, width, dataIn, dataOut);
	//console.log("Worker " + id + " done");
	postMessage([id]);
    };
