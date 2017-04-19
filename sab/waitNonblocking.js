// Polyfill for Atomics.waitNonblocking()
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
	// Don't convert arguments more than once, and signal conversion errors early.
	// The Atomics.load type checks ia -- let's just assume index 0 is valid.

	Atomics.load(ia, 0);
	let index = index_|0;
	let value = value_|0;
	let timeout = timeout_ === undefined ? Infinity : +timeout_;

	// Range checking for the index.

	ia[index];

	// Always do the waiting in the helper, for now.  We could lift certain
	// operations into this function for optimization, for example, we could
	// micro-wait here, or we could check for the not-equal case.

	let h = allocHelper();
	return new Promise(function (resolve, reject) {
	    h.onmessage = function (ev) {
		switch (ev.data[0]) {
		case 'ok':
		    resolve(ev.data[1]);
		    break;
		case 'error':
		    reject(ev.data[1]);
		    break;
		}
		freeHelper(h);
	    }
	    h.postMessage(['wait', ia, index, value, timeout]);
	})
    }
})();

