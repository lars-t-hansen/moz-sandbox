var w = new Worker('sab-slave.js');
var x = new SharedArrayBuffer(0x1000);
var y = new Uint8Array(x);
console.log("Hi");
w.onmessage = 
    function (event) {
	console.log("Master");
	console.log(y[0]);
    };
y[0] = 37;
console.log(x.byteLength);
w.postMessage(x, [x]);		// SharedArrayBuffer must be "transfered", but it is actually shared
console.log(x.byteLength);
