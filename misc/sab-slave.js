onmessage =
    function (event) {
	console.log("Slave");
	var a = event.data;
	console.log(typeof a);
	var y = new Uint8Array(a);
	console.log(y[0]);
	y[0] = 42;
	postMessage("done");
    };
