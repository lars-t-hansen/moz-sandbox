// Like Multicore.js but on SharedArrayBuffer / SharedTypedArray.
//
// Thought experiment.
//
// The idea: code gets included with worker code and also the main
// thread.  The main thread gets designated the master, the other are
// slaves.  To perform a computation just call Multicore.build() on
// the master with a function /name/, not a function /object/.
//
// TODO: Typed arrays are 1D.  So what about higher dimensionality?
// We can in principle provide an additional argument to the function
// that defines how build will treat its argument, this will be the
// dimension map of the array (actual dimensions).  Thus an 2x3x4
// array has a map [2,3,4].  Accessing at (a,b,c) means accessing at
// element a*12 + b*4 + c.  The trick is to inject those constants as
// constants, or as non-varying names, in the worker.  This can be
// fixed with closures and specialization, plus maybe new
// optimizations.

function Multicore_setup(arrayOfSlaves) {
}

function Multicore_build() {
}

function Multicore_splitDimensions() {
}

var Multicore = {
    build: Multicore_build,
    splitDimensions: Multicore_splitDimensions,

    // Directives pertaining to how to process a dimension
    // within Multicore.build()
    SPLIT: -1,

    // Directives pertaining to how to create a schedule for Multicore.build()
    DEFAULT: 0,	                // Element computations are of unknown cost - using 0 allows BALANCED to be used by itself
    COARSE: 1,			// Element computations are expensive
    FINE: 2,			// Element computations are cheap
    TASKS: 3,			// One element per slice (small output array, sized for task parallelism) (not valid input to splitDimensions?)
    BALANCED: 256		// Flag: element computations are all the same cost
};
