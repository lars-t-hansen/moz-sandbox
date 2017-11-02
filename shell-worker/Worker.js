// Worker abstraction for JS shells.
//
// This is a polyfill / self-hosted implementation for SpiderMonkey's JS shell.
// Load this in your JS application before other code.
//
// API
//
// On the main thread:
//
//   let w = new Worker(source_text)     // source_text is literal text, name a String
//   w.postMessage(object)               // serialize object and send to worker
//   w.onmessage = function (ev) ...     // ev.data is a value
//   w.terminate()                       // tell the worker to stop processing events
//   Worker.enterEventLoop()             // start dispatching messages from workers
//   Worker.exitEventLoop()              // causes eventLoop() to return
//
// On the worker:
//
//   postMessage(object)                 // send back to parent
//   onmessage = function (ev) { ... }   // receive from parent, ev.data has datum
//
// The event loop is implicit on the worker for reasons of symmetry with a web
// browser, but explicit on the main thread because, in a shell setting, the
// main thread runs to completion and then the shell exits.

// One 'm2w' (master-to-worker) SAB for each worker

const M2W_MESSAGES = 0;		// Pending master-to-worker messages
const M2W_DONE = 1;		// When set, worker should terminate
const M2W_NUMBYTES = 2 * Int32Array.BYTES_PER_ELEMENT;

// One 'w2m' (worker-to-master) SAB to share

const W2M_ID = 0;		// New worker's ID appears here
const W2M_MESSAGES = 1;		// Pending worker-to-master messages
const W2M_NUMBYTES = 2 * Int32Array.BYTES_PER_ELEMENT;

const workers = [null];		// Worker 0 is not defined
const w2m = new Int32Array(new SharedArrayBuffer(W2M_NUMBYTES));

let exiting = false;		// Set when leaving the main event loop

function Worker(source_text) {
    const id = workers.length;
    const m2w = new Int32Array(new SharedArrayBuffer(M2W_NUMBYTES));
    const timeout = 5000;

    putMessage(id, [w2m, m2w]);
    Atomics.write(w2m, W2M_ID, id);

    evalInWorker(
	`
	const [_w2m, _m2w] = getMessage(${id});

	function onmessage(ev) {}

	function postMessage(msg) {
	    putMessage(0, [${id}, msg]);
	    Atomics.add(_w2m, ${W2M_MESSAGES}, 1);
	    Atomics.wake(_w2m, ${W2M_MESSAGES});
	}

	Atomics.store(_w2m, ${W2M_ID}, 0);
	Atomics.wake(_w2m, ${W2M_ID});
	;
        ${source_text}
        ;
	for(;;) {
	    Atomics.wait(_m2w, ${M2W_MESSAGES}, 0);
	    let msg = getMessage(${id});
	    if (Atomics.load(_m2w, ${M2W_DONE}))
		break;
	    Atomics.sub(_m2w, ${M2W_MESSAGES}, 1);
	    try {
		onmessage({data:msg});
	    } catch (e) {
		print("WORKER ${id}:\n" + e);
	    }
	}
        `
    );

    if (Atomics.wait(w2m, W2M_ID, id, timeout) == "timed-out")
	throw new Error("Worker handshake timed out");

    this._id = id;
    this._m2w = m2w;
    workers.push(this);
}

Worker.prototype.postMessage = function (msg, transfer) {
    putMessage(this._id, msg, transfer);
    Atomics.add(this._m2w, M2W_MESSAGES, 1);
    Atomics.wake(this._m2w, M2W_MESSAGES);
}

Worker.prototype.onmessage = function (ev) {}

Worker.prototype.terminate = function () {
    Atomics.store(this._m2w, M2W_DONE, 1);
    Atomics.wake(this._m2w, M2W_MESSAGES);
}

Worker.enterEventLoop = function () {
    while (!exiting) {
	Atomics.wait(this._w2m, W2M_MESSAGES, 0);
	let [w, msg] = getMessage(0);
	Atomics.sub(this._w2m, W2M_MESSAGES, 1);
	try {
	    workers[w].onmessage({data: msg});
	} catch (e) {
	    print("MAIN:\n" + e);
	}
    }
}

Worker.exitEventLoop = function () {
    exiting = true;
}
