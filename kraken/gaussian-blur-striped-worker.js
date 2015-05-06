importScripts("gaussian-computation.js");

setup();

onmessage =
    function (ev) {
	var [mem, id, height, width] = ev.data;
	console.log("Worker " + id + " starting: " + [id, height, width].join(","));
	var dataIn = new Uint8Array(mem);
	var dataOut = new Uint8Array(dataIn.length);
	computeBlur(0, height, height, width, dataIn, dataOut);
	console.log("Worker " + id + " done");
	postMessage([dataOut.buffer, id], [dataOut.buffer]);
    };
