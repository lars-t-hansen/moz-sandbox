// Include this prefix at the start of the worker code.

// The idiom for blocking receive is:
//
//    while (!selfTask().receive())
//        selfTask().sleep();
//
// A task can receive a message (an Array object of values) or a
// signal (which is reified as a TaskSignal exception).

var selfTask;
var TaskSignal;

onmessage = 
    (function() {
	var Array = this.Array;
	var Function = this.Function;
	var global = this;
	var invoke = ...;	       // Function that reliably invokes a function on an array of arguments
	var tasks = {};		       // Sparse; properties are deleted when no longer needed
	var currentTask;	       // Task structure currently running, or null when on scheduler

	function TaskSignal(v) {
	    this.message = v;
	}

	function WorkerTask(name, id) {
	    this.name_ = name;		// Name meaningful to the application that created the task
	    this.id_ = id;		// Task ID, globally unique, internal
	    this.queue_ = [];		// Message queue - needed because messages can arrive while task is sleeping for a time
	    this.co_ = null;		// Coroutine
	}

	WorkerTask.prototype.receive = 
	    function() {
		if (this.queue.length == 0)
		    return null;
		return this.queue.shift();
	    };

	WorkerTask.prototype.yield =
	    function () {
		// ah, how do we know there are messages pending?  we don't.
		// the worker needs an API for that?  We can emulate it by
		// sending a pingback message to the master so that we can
		// yield to the event loop and be sure that there's a message
		// waiting.  (Can we just post a message to self?)
		yieldToScheduler();
	    };

	WorkerTask.prototype.sleep =
	    function (ms) {
		if (typeof ms === "number")
		    ms = Math.max(0, ms|0);
		else
		    ms = 0;
		if (ms > 0)
		    setupWakeupCall(currentTask, ms);
		yieldToScheduler();
	    };

	function signalTaskTerminated(t) {
	    var id = t.id_;
	    t.name_ += "***TERMINATED***";
	    t.id_ = -1;
	    t.queue_ = null;
	    delete tasks[t.id_];
	    sendToMaster(["terminated", id]);
	}

	function setupWakeupCall() {
	    // Either post timeout message on self, or post one on master so that master
	    // will wake us up.
	    ...;
	}
 
	function yieldToScheduler() {
	    var v = Coroutine_yieldTo(null, currentTask);
	    if (v instanceof TaskSignal)
		throw v;
	}

	function schedulerEventHandler(event) {
	    var d = event.data;
	    if (Array.isArray()) {
		switch (d[0]) {
		case "run": 
		    // Create a new task on this worker and start it.
		    // ["run" task-id function-name arg ...]
		    if (d.length < 3 || !isNumber(d[1]) || !isString(d[2]) || typeof global[d[2]] != "function")
			console.log("Bad argument(s) to 'run' message");
		    else {
			var id = d[1];
			var fn = global[d[2]];
			var args = d.shift(3);
			tasks[id] = t;
			var t = new WorkerTask(d[2], id, new Coroutine(function () { 
			    invoke(fn, args);
			    signalTaskTerminated(t);
			}));
		    }
		    break;
		case "send":
		    // Send a message to a task on this worker.  The task is
		    // woken; if it is waiting for a message then the message
		    // is delivered, otherwise the message is queued and can
		    // be read.  Note 
		    // ["send" receiving-task-id sending-task-id value ...]
			...;
		    break;
		case "signal":
		    // ["signal" receiving-task-id value]
			...;
		default:
		    console.log("Worker message loop: Unknown message tag: " + d[0]);
		    break;
		}
	    }
	    else
		console.log("Worker message loop: Unknown message: " + d);
	}

	global.TaskSignal = TaskSignal;
	global.selfTask = function () { return currentTask };

	return schedulerEventHandler;
    })();
