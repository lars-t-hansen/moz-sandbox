// Polyfill for Atomics.waitNonblocking() for web browsers
//
// Any kind of agent that is able to create a new Worker can use this polyfill.
//
// Load this file in all agents that will use Atomics.waitNonblocking.
//
// Agents that don't call Atomics.waitNonblocking need do nothing special.
//
// Any kind of agent can wake another agent that is sleeping in
// Atomics.waitNonblocking by just calling Atomics.wake for the location being
// slept on, as normal.
//
// In this polyfill, Atomics.waitNonblocking is not very fast.

// Design considerations:
//
// An agent can have multiple nonblocking waits outstanding at the same time,
// and can be woken for them in any order.
//
// Indeed those multiple waits can be on the same location.

// Implementation:
//
// For every wait we fork off a Worker to perform the wait.  Workers are reused
// when possible.  The worker communicates with its parent using postMessage.

(function () {
    let helperCode = `
    onmessage = function (ev) {
	try {
	    switch (ev.data[0]) {
	    case 'wait': {
		let [_, ia, index, value, timeout] = ev.data;
		let result = Atomics.wait(ia, index, value, timeout)
		postMessage(['ok', result]);
		break;
	    }
	    default:
		throw new Error("Bogus message sent to wait helper: " + e);
	    }
	} catch (e) {
	    console.log("Exception in wait helper");
	    postMessage(['error', 'Exception']);
	}
    }
    `;

    let helpers = [];

    function allocHelper() {
	if (helpers.length > 0)
	    return helpers.pop();
	let h = new Worker("data:application/javascript," + encodeURIComponent(helperCode));
	return h;
    }

    function freeHelper(h) {
	helpers.push(h);
    }

    // Atomics.waitNonblocking always returns a promise.  Throws standard errors
    // for parameter validation.  The promise is resolved with a string as from
    // Atomics.wait, or, in the case something went completely wrong, it is
    // rejected with an error string.

    Atomics.waitNonblocking = function (ia, index_, value_, timeout_) {
	if (typeof ia != "object" || !(ia instanceof Int32Array) || !(ia.buffer instanceof SharedArrayBuffer))
	    throw new TypeError("Expected shared memory");

	// These conversions only approximate the desired semantics but are
	// close enough for the polyfill.

	let index = index_|0;
	let value = value_|0;
	let timeout = timeout_ === undefined ? Infinity : +timeout_;

	// Range checking for the index.

	ia[index];

	// Always do the waiting in the helper, for now.
	//
	// Possible optimization: if ia[index] != value then just set up an
	// immediate callback that will resolve with "not-equal", without
	// involving the helper thread.  In a browser this would be a
	// setTimeout(f, 0) presumably.

	return new Promise(function (resolve, reject) {
	    let h = allocHelper();
	    h.onmessage = function (ev) {
		// Free early so that it can be reused if the resolution needs a helper.
		freeHelper(h);
		switch (ev.data[0]) {
		case 'ok':
		    resolve(ev.data[1]);
		    break;
		case 'error':
		    // Note, rejection is not in the spec, it is an artifact of the polyfill.
		    // The helper already printed an error to the console.
		    reject(ev.data[1]);
		    break;
		}
	    }
	    h.postMessage(['wait', ia, index, value, timeout]);
	})
    }
})();

