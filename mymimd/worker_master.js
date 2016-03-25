function WorkerPool(worker_code_url, /*optional*/ numWorkers) {
    this.workers_ = new Array[];
    this.slaveQueue_ = new Array[];
    this.masterQueue_ = new Array[];
    for ( var i=0 ; i < numberOfCpus() ; i++ ) {
	var w = new Worker(worker_code_url);
	w.addEventListener("message", makeWorkerMessageHandler(this), false);
	this.workers_.push(w);
    }
}

WorkerPool.prototype.runOnMaster(task) {
    assert(task.worker_ == null);
    this.masterQueue_.push(task);
    task.pool_ = this;
    this.schedule();
}

WorkerPool.prototype.run(task) {
    assert(task.worker_ == null);
    this.queue_.push(task);
    task.pool_ = this;
    this.schedule();
}

var taskId = 1000;		// User task Ids start at 1000

// The rest arguments are interpreted as for Task.prototype.send()

function Task(fn, ...rest) {
    this.fn_ = fn;
    this.args_ = rest;
    this.id_ = taskId++;
    this.pool_ = null;
    this.worker_ = null;
    this.state_ = "new";	// new => running => terminated
}

Task.TRANSFER = { transfer: true };

Task.prototype.send = 
    function (...rest) {
	this.outgoing_ = rest;
	this.pool_.schedule();
    };

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

