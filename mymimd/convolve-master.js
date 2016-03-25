//$$include "worker_master.js"

// General approach:
//  - use filepicker / blob / FileReader to get the file
//  - convolve it
//  - display it in a canvas as individual pixels

var pool = new WorkerPool("convolve-worker.js", writeGrid);

const { loc, bytes, height, width, maxval } = readPgm("cat.pgm");

pool.run(new Task("coordinator"), pool, 10, 10, Task.TRANSFER, grid.buffer);

function readGrid(file) {
}

function writeGrid(grid) {
}
