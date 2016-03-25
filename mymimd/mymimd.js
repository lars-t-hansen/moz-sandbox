var myWorker = new Worker("mimd_worker.js");

var k = 0;
myWorker.addEventListener("message", function (oEvent) {
    if (oEvent.data == "ho")
	console.log(sab[0]);
    console.log("Called back by the worker! " + (k++));
}, false);

myWorker.postMessage("ali");
myWorker.postMessage(sab.buffer, [sab.buffer]);

function WorkerPool(worker_code, main, /*optional*/ numWorkers) {
    // Adding a prefix is hard
    this.main_ = main;
    this.workers_ = new Array[];
    for ( var i=0 ; i < numberOfCpus() ; i++ ) {
	var w = new Worker(worker_code);
	w.addEventListener("message", makeWorkerMessageHandler(this), false);
	this.workers_.push(w);
    }
}

WorkerPool.prototype.mainTask = function() { return this.main_ }

// Put the task on the run queue

WorkerPool.prototype.run(task) {
    assert(task.worker_ == null);
    // add to queue
    // run scheduler, which will send work if there's work
}

// Make the task known but it will not do anything until it receives a message,
// at which point it will be scheduled with the received data

WorkerPool.prototype.idle(task) {
    ...
}

function makeWorkerMessageHandler(pool) {
    return function(event) {
	// event.data is always an array
	// the first element of that array is a key
	// the second element of that array is the sending task (an integer)
	// the remaining are data
	// the key is either a "send" to another task, or an "idle", which says the worker is wanting more work
	var d = event.data;
	switch (d[0]) {
	case "send":
	    // receiver (a task ID) is first datum
	    // arguments are remaining data
	    pool.sendTo(...);
	    break;
	case "idle": 
	    pool.schedule(task); // how do we find the task?
	    break;
	default:
	    ...;
	}
    }
}

//// User code
////
//// Rough design is that the user code operates mainly on tasks which it schedules on a worker pool.
//// 

var pool = new WorkerPool("worker_code.js", mainTaskCode);

var numPieces = 100;
var receviedPieces = 0;
for ( var h=0 ; h < 10 ; h++ )
    for ( var w=0 ; w < 10 ; w++ )
	pool.run(new Task("convolve", pool.mainTask(), h, w, copyDataOut(h, w)));

function mainTaskCode(h, w, data) {
    // collect the results
    copyDataIn(h, w, data);
    if (++receivedPieces == numPieces)
	saveDataToFile();
}

//// Worker code

function convolve(sender, h, w, data) {
    // sender is a Task
    // compute the data ...
    var computedData = ...;
    sender.send(h, w, computedData);
}
