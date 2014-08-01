//////////////////////////////////////////////////////////////////////
//
// Worker code:
//
// Each element comprises 3 10-bit fields.  The final value of each element should be (iter << 20) | (iter << 10) | iter,
// if mutual exclusion works OK.

importScripts("atomics.js");
importScripts("mutex.js");
importScripts("critical-parameters.js");

var iter = 0;			// Global iteration counter

var id;				// Worker identity (0..workers-1)
var sia;			// Int32Array
var m;				// Mutex

function compute() {
    if (iter == iterations) {
	console.log("Done " + id);
	Atomics.or(sia, statusLoc, 1 << id);
	return;
    }

    iter++;
    if (doLock)
	m.lock();
    for ( var x=arrayLo ; x < arrayLim ; x++ )
	sia[x] += (1 << id*10);
    if (doLock)
	m.unlock();
    setTimeout(compute, 0);
}

onmessage = function (ev) {
    if (!ev.data)
	return;
    var msg = ev.data;
    id = msg.id;
    sia = new Int32Array(msg.sab);
    m = new Mutex(sia, mutexLoc);
    console.log("Running " + id);
    setTimeout(compute, 0);
}
