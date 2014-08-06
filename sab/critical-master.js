// Program that tests simple mutual exclusion (master).

// A shared buffer is allocated and shared between three workers.
// Each worker treats the buffer as an int32 array, each element is
// treated as three 10-bit fields.  Each time a worker enters the
// critical section it increments its bitfield within each element.
//
// Each time the worker leaves the critical section it attempts to
// yield the scheduler.
//
// At the end, all bit fields of all elements should have the number
// of iterations executed.  (Otherwise, mutual exclusion was not
// achieved.)  We should be able to observe failures by turning the
// locks into no-ops.

importScripts("critical-parameters.js");

var nbytes = 4*(arrayElements + arrayExtra) + 4095 & ~4095;
var sab = new SharedArrayBuffer(nbytes);

var sia = new Int32Array(sab);
sia[mutexLoc] = 0;
sia[statusLoc] = 0;
for ( var x=arrayLo ; x < arrayLim ; x++ )
    sia[x] = 0;

onmessage = function (ev) {
    var ws = [];
    for ( var x=0 ; x < workers ; x++ )
	ws.push(new Worker("critical-worker.js"));

    for ( var w in ws )
	ws[w].postMessage({id: w, sab: sab}, [sab]);

    waitUntilDone();
}

var attempts = 0;
var laststat = -1;
function waitUntilDone() {
    var c = Atomics.load(sia, statusLoc);
    if (c != laststat) {
	console.log("Master status = " + c);
	laststat = c;
    }
    if (c != (1 << workers)-1) {
	if (attempts++ > maxPollAttempts) {
	    console.log("Giving up - takes too long!");
	    return;
	}
	setTimeout(waitUntilDone, pollTimeout);
	return;
    }

    var correct = (iterations << 20) | (iterations << 10) | iterations;
    var bogus = 0;
    var numfail = 0;
    for ( var x=arrayLo ; x < arrayLim ; x++ )
	if (sia[x] != correct) {
	    if (++bogus < 20)
		console.log("Incorrect @ " + x + ": expected " + correct + "; got " + sia[x]);
	    numfail++;
	}

    if (numfail)
	console.log("Failed!  Number of failures = " + numfail);
    else {
	console.log("Success!");
	var n = sia[arrayLo];
	for ( var i=0 ; i < workers ; i++ ) {
	    console.log("Worker " + i + ": " + (n & 1023).toString());
	    n >>= 10;
	}
    }
}
