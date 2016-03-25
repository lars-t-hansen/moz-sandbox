//$$include "worker_prefix.js"

// Tile size is ((height-2)/htiles) x ((width-2)/wtiles) except for the rightmost
// and bottommost tiles, which also absorb the slop.  Each tile is copied
// by the coordinator into a grid that has a one-element borter on each side, copied from the
// input buffer.  Each tile is convolved separately and then the tiles are pasted
// back together by a coordinator (also in a worker).

function coordinator(self, pool, htiles, wtiles, data) {

    // Create tasks and scatter work.

    var grid = new Uint8Array(data);
    var [loc, width, height] = parseGrid(grid);
    var hsize = (height-2)/htiles;
    var wsize = (width-2)/wtiles;
    for ( var h=0 ; h < htiles ; h++ ) {
	for ( var w=0 ; w < wtiles ; w++ ) {
	    var upper = hsize*h + 1;
	    var left = wsize*w + 1;
	    var lower = h == htiles-1 ? height-1 : upper+hsize;
	    var right = w == wtiles-1 ? width-1 : left+wsize;
	    pool.run(new Task("convolve"), 
		     coord, 
		     h, 
		     w, 
		     Task.TRANSFER,
		     copySubgridOut(grid, loc, height, width, upper, left, lower, right).buffer);
	}
    }

    // Gather results.
    // 
    // This might normally be a "receive" loop; for now the
    // receive loop is implicit since we don't have coroutines yet.

    var numPieces = htiles * wtiles;
    self.setReceiver(
	function (h, w, data) { 
	    var [h, w, data] = self.receive();
	    var upper = hsize*h + 1;
	    var left = wsize*w + 1;
	    var lower = h == htiles-1 ? height-1 : upper+hsize;
	    var right = w == wtiles-1 ? width-1 : left+wsize;
	    copySubgridIn(grid, loc, height, width, upper, left, lower, right, data);

	    if (--numPieces == 0) {
		pool.send(Task.TRANSFER, result.buffer);
		self.setReceiver(null);
	    }
	});
}

// Convolver core

function convolve(self, coordinator, h, w, data) {
    // sender is a Task
    // compute the data ...
    var computedData = ...;
    coordinator.send(h, w, Task.TRANSFER, computedData.buffer);
}

