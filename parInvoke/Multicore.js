// Polyfill for the Multicore object
//
// 2014-06-21 / lhansen@mozilla.com

// Major API changes:
//  2014-06-21: Introduced Multicore.splitDimensions()
//  2014-06-21: Removed the 'origin' parameter to Multicore.build()
//              and required the iteration space to have the same
//              length as the rank of the output object.  Explicit
//              splitting can be use to accomplish what partial
//              iteration did.

// Summary of the API defined in this file:
//
// Multicore.build()
//   Invokes its function argument in parallel across multiple worker
//   threads, producing values into an existing output volume.
//
// Multicore.splitDimension()
//   Computes a reasonable number of slices when given dimension sizes
//   and an estimate of the cost of a kernel.
//
// Multicore.SPLIT
//   Directive used within the iteration spec of Multicore.build().
//
// Multicore.DEFAULT
// Multicore.COARSE
// Multicore.FINE
// Multicore.TASKS
//   Hints provided to Multicore.build() and Multicore.splitDimension().
//   Multicore.DEFAULT has a known value of zero.
//
// Multicore.BALANCED
//   A flag to be or'ed into the hints.


//////////////////////////////////////////////////////////////////////
//
// Multicore.build()
//
// Invokes its function argument in parallel across multiple worker
// threads, producing values into an existing output volume.  This is
// effectively a concurrent multidimensional iteration primitive with
// some tuning knobs.
//
// Signature:
//   Multicore.build(fn, obj, iterSpace, hint) => newobj
//
// Arguments:
//   'fn' must be a callable (taking arguments described later).
//
//   'obj' is a TypedObject array of rank R >= 1 and dimension lengths
//     L_0 .. L_{R-1}, or an Array with rank R == 1 and length L_0.
//
//     If obj is a TypedObject array then it will be neutered as part
//     of this process.  If obj is an Array then its length will be
//     set to zero.  This neutering will take place before the first
//     invocation of fn.
//
//   'iterSpace' must be an array whose length I equals R, consisting
//     entirely of arrays.  The subarrays must be of length 2, 3, or 4.
//
//     The two first elements must be integers, the first no greater
//     than the second (note they can be negative).
//
//     If there is a third element it must be the value of
//     Multicore.SPLIT.  If a subarray has such a SPLIT directive,
//     then all subarrays preceding it must have the directive too.
//
//     The fourth element, if present, is the desired slice size (in
//     number of elements) along that direction; the scheduler will
//     obey this closely (though there may be upper and lower limits
//     on it).
//
//     The two first values of the inner arrays of iterSpace define
//     the bounds B_D for iteration within each dimension D (outermost
//     dimension leftmost in iterSpace).  We require 0 <= B_D < L_D
//     for all D.
//
//   'hint' must be either undefined or one of the predefined hints
//     Multicore.COARSE, Multicore.FINE, or Multicore.DEFAULT.  If
//     undefined it is interpreted as Multicore.DEFAULT.  The hint is
//     used to guide the scheduler but is ignored for SPLIT dimensions
//     that have an explicit slice size.
//
// Return value:
//   'newobj' is a fresh object of the same type and size as obj.
//
//   newobj will contain all the indexed values of obj, except at
//   locations within the storage bounds; those locations will contain
//   fresh values computed as described below.
//
//   Performance note: To the greatest extent possible, the newobj
//   will simply assume ownership of the storage of obj.
//
// Operation:
//   The iteration is performed as follows.
//
//   If no dimensions are split, then:
//
//     Fn is passed R values representing the index of the element
//     that is to be computed and must return the value that will be
//     stored in newobj.
//
//   If any dimensions are split, then:
//
//     Iteration proceeds over rank-R subvolumes whose sizes are
//     computed according to the parameters of the SPLIT directives,
//     the hint parameter, and internal defaults.
//
//     Specifically, fn is passed R values representing the smallest
//     (think top-left-nearest) index of the subvolume that is to be
//     computed as well as an origin-zero rank-R array representing
//     that subvolume.  Fn can freely read and write this array at any
//     location; initial values are "reasonable defaults" (zero for
//     numbers, null for pointers, and undefined for Any and in
//     Arrays).  Fn need not write all values in the array.  The
//     return value of fn is ignored.
//
//     If outer dimensions are SPLIT but inner dimensions are not then
//     the subvolumes will cover entire index ranges in the unsplit
//     dimensions.
//
//     For example, consider the 2D case.  If both dimensions are
//     SPLIT then fn will be passed "tiles" comprising several partial
//     rows.  If only the outer dimension is SPLIT then fn will be
//     passed "strips" comprising several complete rows.
//
//   In all cases, invocations of fn are independent and no invocation
//   of fn can observe values written by other invocations.
//
//   The slice sizes across all dimensions, if not given as parameters
//   along with a SPLIT directive, are computed more or less as by
//   Multicore.splitDimension() using the sizes of the iteration
//   ranges and the hints provided to Multicore.build().
//
//   (In practice, Multicore.build() has access to an accurate core
//   count and may do better than Multicore.splitDimensions().)
//
//   If a slice size is provided as a parameter to a SPLIT directive
//   then it will be respected.

function Multicore_build(fn, obj, iterSpace, hint)
{
    ////////////////////////////////////////////////////////////
    // Polyfill for internal error reporting machinery
    const JSMSG_MISSING_FUN_ARG = "Missing function argument";
    const JSMSG_NOT_FUNCTION = "Not a function";
    const JSMSG_BAD_TYPE = "Bad type";
    const JSMSG_BAD_ARRAY_LENGTH = "Bad array length";
    const JSMSG_BAD_INDEX = "Bad index";
    const JSMSG_BAD_VALUE = "Bad value";

    function ThrowError(id, ...xs) { throw new Error(id); }
    // End polyfill of error reporting
    ////////////////////////////////////////////////////////////

    const SPLIT = -1;          // Value of Multicore.SPLIT
    const DEFAULT = 0;         // Value of Multicore.DEFAULT
    const COARSE = 1;          // Value of Multicore.COARSE
    const FINE = 2;	       // Value of Multicore.FINE
    const TASKS = 3;
    const BALANCED = 256;
    const FLAGMASK = ~255;

    const PRETEND_TO_NEUTER = true; // More performant, OK for now

    var R=0, isArray=false, isTypedObj=false, coarse=false, splitCount=0, splits=null, defaultSplitSize=0;

    if (arguments.length < 3)
        ThrowError(JSMSG_MISSING_FUN_ARG, arguments.length+1, 'Multicore.build');

    if (!IsCallable(fn))
	ThrowError(JSMSG_NOT_FUNCTION, DecompileArg(0, fn));

    // Test ordering is careful at the moment, but that will be fixed
    if (IsTypedObjectArray(obj)) {
	R = TypedObjectArrayRank(obj);
	isTypedObj = true;
    }
    else if (IsTrueArray(obj)) {
        R = 1;
	isArray = true;
    }
    else
	ThrowError(JSMSG_BAD_TYPE + 1, obj);

    if (!IsTrueArray(iterSpace))
	ThrowError(JSMSG_BAD_TYPE + 2, iterSpace);

    // Checking hint before iterSpace because we will use the hint
    if (hint === undefined)
	hint = DEFAULT;
    // TODO: Want to randomize the defaults a little to avoid code depending on them?
    switch (hint & ~FLAGMASK) {
    case TASKS:
	defaultSplitSize = 1;
	break;
    case COARSE:
	defaultSplitSize = 1;
	break;
    case FINE:
	defaultSplitSize = 64;
	break;
    case DEFAULT:
	defaultSplitSize = 32;
	break;
    default:
	ThrowError(JSMSG_BAD_VALUE, hint);
    }

    if (iterSpace.length != R)
	ThrowError(JSMSG_BAD_ARRAY_LENGTH);

    for (var i=0; i < R; i++) {
	var v = iterSpace[i];
	if (!IsTrueArray(v))
	    ThrowError(JSMSG_BAD_TYPE + 3, v);
	if (v.length < 2 || v.length > 4)
	    ThrowError(JSMSG_BAD_ARRAY_LENGTH);
	if (!IsInt32(v[0]) || !IsInt32(v[1]) || v[0] > v[1])
	    ThrowError(JSMSG_BAD_VALUE, v);
	if (v.length >= 3) {
	    if (v[2] == SPLIT) {
		if (!splits)
		    splits = [];
		if (splitCount < i)
		    ThrowError(JSMSG_BAD_TYPE + 4, v);
		splitCount++;
		if (v.length == 4) {
		    if (!IsInt32(v[3]) || v[3] <= 0)
			ThrowError(JSMSG_BAD_VALUE, v);
		    splits.push(v[3]);
		}
		else
		    splits.push(defaultSplitSize);
	    }
	    else
		ThrowError(JSMSG_BAD_VALUE, v);
	}
    }

    if (isArray && !IsPackedArray(obj))
	MakePackedArray(obj);

    var result = NeuterObjectAndClone(obj);

    switch (R) {
    case 1:
	var lo=iterSpace[0][0];
	var hi=iterSpace[0][1];
	if (lo < 0 || hi > result.length)
	    ThrowError(JSMSG_BAD_INDEX);
	return splitCount ? exact1split(lo, hi) : exact1(lo, hi);
    case 2:
	var lo0=iterSpace[0][0];
	var hi0=iterSpace[0][1];
	var lo1=iterSpace[1][0];
	var hi1=iterSpace[1][1];
	if (lo0 < 0 || hi0 > result.length ||
	    lo1 < 0 || hi1 > DimLength(result, 1))
	{
	    ThrowError(JSMSG_BAD_INDEX);
	}
	return splitCount ? exact2split(lo0, hi0, lo1, hi1) : exact2(lo0, hi0, lo1, hi1);
    case 3:
	var lo0=iterSpace[0][0];
	var hi0=iterSpace[0][1];
	var lo1=iterSpace[1][0];
	var hi1=iterSpace[1][1];
	var lo2=iterSpace[2][0];
	var hi2=iterSpace[2][1];
	if (lo0 < 0 || hi0 > result.length ||
	    lo1 < 0 || hi1 > DimLength(result, 1) ||
	    lo2 < 0 || hi2 > DimLength(result, 2))
	{
	    ThrowError(JSMSG_BAD_INDEX);
	}
	return splitCount ? exact3split(lo0, hi0, lo1, hi1, lo2, hi2) : exact3(lo0, hi0, lo1, hi1, lo2, hi2);
    default:
	throw new Error("Internal error: only up to 3 dimensions supported"); // FIXME
    }
    return null;		// Dumb compiler

    function exact1(lo, hi) {
	for (; lo < hi; lo++)
	    result[lo] = fn(lo);
	return result;
    }

    function exact1split(lo, hi) {
	var range = hi-lo;
	if (range == 0)
	    return result;
	var sliceSize = splits[0];
	var sliceCount = Math.ceil(range/sliceSize);
	var lastSize = range % sliceSize;
	if (lastSize == 0) {
	    sliceCount--;
	    lastSize = sliceSize;
	}
	if (isArray) {
	    // For the polyfill we cannot share Array storage between
	    // the slice passed to the worker and the result.  So make
	    // a temp array for the slice and pass that in, and copy
	    // the values out.
	    var slice = new Array(sliceSize);
	    for (var id=0 ; id < sliceCount ; id++ ) {
		var baseIndex = lo + id*sliceSize;
		var len = id < sliceCount-1 ? sliceSize : lastSize;
		slice.length = len;
		for ( var i=0 ; i < len ; i++ )
		    slice[i] = undefined;
		fn(baseIndex, slice);
		for ( var i=0 ; i < len ; i++ )
		    result[baseIndex+i] = slice[i];
	    }
	}
	else {
	    // Ditto here, for now - but want to do better soon with a
	    // shared-subarray idea, or even better, a cursor like the
	    // one the builtins are using.  (There's nothing
	    // inherently bad about a cursor provided it can be
	    // rendered inert.)
	    var T = TypedObject.objectType(result).elementType
	    var stdSlice = new (T.array(sliceSize))();
	    var lastSlice = lastSize < sliceSize ? new (T.array(lastSize))() : stdSlice;
	    for (var id=0, baseIndex=lo ; id < sliceCount ; id++, baseIndex += sliceSize ) {
		var slice = (id == sliceCount-1 ? lastSlice : stdSlice);
		clear1D(slice);
		fn(baseIndex, slice);
		blit1D(result, baseIndex, slice);
	    }
	}
	return result;
    }

    function exact2(lo0, hi0, lo1, hi1) {
	for (; lo0 < hi0; lo0++)
	    for (var i=lo1; i < hi1; i++)
		result[lo0][i] = fn(lo0, i);
	return result;
    }

    function exact2split(lo0, hi0, lo1, hi1) {
	// Splitting can happen along one or two dimensions; we end up
	// with strips and tiles, respectively.
	var range0 = hi0-lo0;
	var range1 = hi1-lo1;
	if (range0 == 0 || range1 == 0)
	    return result;

	var sliceSize0 = splits[0];
	var sliceCount0 = Math.ceil(range0/sliceSize0);
	var lastSize0 = range0 % sliceSize0;
	if (lastSize0 == 0)
	    lastSize0 = sliceSize0;

	// If we split along the first dimension but not the second,
	// then produce strips: tiles can be produced by actually
	// splitting along the second dimension.

	var sliceSize1 = splitCount < 2 ? range1 : splits[1];
	var sliceCount1 = Math.ceil(range1/sliceSize1);
	var lastSize1 = range1 % sliceSize1;
	if (lastSize1 == 0)
	    lastSize1 = sliceSize1;

	// Again, copy-out for the polyfill.  This must be fixed, as
	// for the 1D case.  We count on creating a subarray of the
	// result that references a subgrid as if it were a 2D array.
	// This means slightly more complicated code in the generated
	// indexing expression since a different row length is used
	// for range checking and offset computation.

	var T = TypedObject.objectType(result).elementType.elementType;
	var stdSlice = new (T.array(sliceSize0,sliceSize1))();
	var bottomSlice = lastSize0 < sliceSize0 ? new (T.array(lastSize0, sliceSize1))() : stdSlice;
	var rightSlice = lastSize1 < sliceSize1 ? new (T.array(sliceSize0, lastSize1)) : stdSlice;
	var bottomRightSlice = lastSize0 < sliceSize0 || lastSize1 < sliceSize1 ? new (T.array(lastSize0, lastSize1)) : stdSlice;

	// This loop nest has minimal testing and branching but may not be such a great idea
	// since fn might have to be inlined four times.
	//
	// On the other hand, the nest can be specialized depending on whether there are
	// odd slices at the edges: for an evenly divisible grid only the one doubly nested
	// loop will be needed.
	//
	// (Also, for concurrent execution this would look different.)

	var id0, id1, baseIndex0, baseIndex1;
	for ( id0=0, baseIndex0=lo0 ; id0 < sliceCount0-1 ; id0++, baseIndex0 += sliceSize0 ) {
	    for ( id1=0, baseIndex1=lo1 ; id1 < sliceCount1-1 ; id1++, baseIndex1 += sliceSize1 ) {
		clear2D(stdSlice);
		fn(baseIndex0, baseIndex1, stdSlice);
		blit2D(result, baseIndex0, baseIndex1, stdSlice);
	    }
	    clear2D(rightSlice);
	    fn(baseIndex0, baseIndex1, rightSlice);
	    blit2D(result, baseIndex0, baseIndex1, rightSlice);
	}
	for ( id1=0, baseIndex1=lo1 ; id1 < sliceCount1-1 ; id1++, baseIndex1 += sliceSize1 ) {
	    clear2D(bottomSlice);
	    fn(baseIndex0, baseIndex1, bottomSlice);
	    blit2D(result, baseIndex0, baseIndex1, bottomSlice);
	}
	clear2D(bottomRightSlice);
	fn(baseIndex0, baseIndex1, bottomRightSlice);
	blit2D(result, baseIndex0, baseIndex1, bottomRightSlice);

	return result;
    }

    function exact3(lo0, hi0, lo1, hi1, lo2, hi2) {
	for (; lo0 < hi0; lo0++)
	    for (var i=lo1; i < hi1; i++)
		for (var j=lo2; j < hi2; j++)
		    result[lo0][i][j] = fn(lo0, i, j);
	return result;
    }

    function exact3split(lo0, hi0, lo1, hi1, lo2, hi2) {
	// FIXME
    }

    // Need a primitive: set cursor to subgrid and clear it
    function clear1D(slice) {
	var len = slice.length;
	for ( var i=0 ; i < len ; i++ )
	    slice[i] = 0; // 0 is really only right for numeric types
    }
    
    function blit1D(result, baseIndex, slice) {
	var len = slice.length;
	for ( var i=0 ; i < len ; i++ )
	    result[baseIndex+i] = slice[i];
    }

    // Need a primitive: set cursor to subgrid and clear it
    function clear2D(slice) {
	var len0 = slice.length;
	var len1 = slice[0].length;
	for ( var i=0 ; i < len0 ; i++ )
	    for ( var j=0 ; j < len1 ; j++ )
		slice[i][j] = 0; // 0 is only right for numeric types
    }

    // Polyfill-only
    function blit2D(result, baseIndex0, baseIndex1, slice) {
	var len0 = slice.length;
	var len1 = slice[0].length;
	for ( var i=0 ; i < len0 ; i++ )
	    for ( var j=0 ; j < len1 ; j++ )
		result[baseIndex0+i][baseIndex1+j] = slice[i][j];
    }

    function IsCallable(x) {
	return x instanceof Function;
    }

    function IsObject(x) {
	return typeof x == "object" && !!x;
    }

    function IsPackedArray(x) {
	return IsTrueArray(x);	// FIXME
    }

    function IsTrueArray(x) {
	return x && Array.isArray(x);
    }

    function IsTypedObjectArray(x) {
	return IsObject(x) && TypedObject.objectType(x) instanceof TypedObject.ArrayType;
    }

    function TypedObjectArrayRank(x) {
	var rank = 0;
	while (TypedObject.objectType(x) instanceof TypedObject.ArrayType) {
	    x = x[0];
	    rank++;
	}
	return rank;
    }

    function DimLength(x, dim) {
	switch (dim) {
	case 0:
	    return x.length;
	case 1:
	    return x[0].length;	// FIXME: wrong if dimension 0 has length 0
	case 2:
	    return x[0][0].length; // FIXME: ditto
	case 3:
	    return x[0][0][0].length; // FIXME: ditto
	default:
	    while (dim-- > 0)
		x = x[0];
	    return x.length;	// FIXME: need something better for sure
	}
	return null;
    }

    function IsInt32(x) {
	return (x|0) === x;
    }

    // Probably insert "undefined" values here
    // Would be useful to have an appropriate default value though
    // Still, this is only for Array
    function MakePackedArray(x) {
	return x;		// This must be some sort of primitive
    }

    // Fundamentally a system operation, the Polyfill can express it, but not
    // terribly well.
    function NeuterObjectAndClone(x) {
	// Close enough to fool casual observers, but not actually correct
	if (PRETEND_TO_NEUTER)
	    return x;

	// Here: make a copy
	// Copy over values that are to be preserved
	// Neuter the input if TypedObject array, or set length to 0 if Array
	// FIXME
	return x;
    }
}

//////////////////////////////////////////////////////////////////////
//
// Multicore.splitDimension()
//
// Compute reasonable slice sizes across one or more dimensions of an
// iteration.  This is intended for use 
//
// Signature:
//   Multicore.splitDimension(size, hint) => object
//
// Arguments:
//   'size' is either the size of the dimension being split (typically
//     the number of elements) or an array of such sizes.  The
//     primitive values of 'size' must be positive.
//
//   'hint' is a hint about the expense of the computation for each
//     element: Multicore.FINE, Multicore.COARSE, or Multicore.DEFAULT;
//     the Multicore.BALANCED flag may be or'ed in to all those.
//
// Return value:
//   If 'size' is a number then returns a new object with these properties:
//     - 'numSlices' is the total number of slices, numSlices > 0
//     - 'sliceSize' is the size of all but the last slice, sliceSize > 0
//     - 'lastSize' is the size of the last slice, lastSize > 0
//
//   If 'size' is an array then returns an array of objects as just
//   outlined, one object per dimension.
//
// The computation is more or less as follows:
//
// - The default is some variation on Shu's algorithm, for example
// - If computations are coarse the slice size is smaller
// - If computations are fine the slice size is larger
// - If computations are unbalanced then the slice size is smaller
// - If computations are balanced the slice size is larger
// - If the available concurrency is low then the slice size is larger
// - If the available concurrency is high then the slice size is smaller
//
// As a result:
//
// - "Balanced, fine" slices will be some small constant times the
//   number of worker threads.
// - "Unbalanced, coarse" slices will be not much larger than 1.
// - Low-core systems will end up with numSlices==1, indicating
//   that a sequential computation is best
//
// Notes:
//
// - The function takes into account the amount of parallelism
//   available without revealing fingerprintable values.  In
//   borderline cases the function may return true from one call and
//   false from another, for no apparent reason.  The returned value
//   may vary with system load and battery level.  Etc.

// TODO: the following is not at all up to spec

function Multicore_splitDimension(size, hint)
{
    if (size == 0)
	throw new Error("Cannot slice a dimension of length 0");

    if (size == 1)
	return {numSlices:1, sliceSize:1, lastSize:1};

    var n = Math.max(1, Math.floor(Math.log2(size)));
    var sz = Math.max(1, Math.floor(size / n));
    if (n*sz > size) {
	if (n > 1)
	    n--;
	else
	    sz--;
    }
    if (n <= 0 || sz <= 0)
	throw new Error("Bogus slices");
    var last = size - sz*n;
    if (last > 0)
	n++;
    //print("Slices: " + n + " " + sz + " " + last);
    return {numSlices:n, sliceSize:sz, lastSize:last};
}

// Private, not exposed
var _corecount = 4;

var Multicore = {
    build: Multicore_build,
    splitDimension: Multicore_splitDimension,

    // Directives pertaining to how to process a dimension
    // within Multicore.build()
    SPLIT: -1,

    // Directives pertaining to how to create a schedule
    // for Multicore.build() or Multicore.splitDimension()
    DEFAULT: 0,	                // Element computations are of unknown cost - using 0 allows BALANCED to be used by itself
    COARSE: 1,			// Element computations are expensive
    FINE: 2,			// Element computations are cheap
    TASKS: 3,			// One element per slice (small output array, sized for task parallelism) (not valid input to splitDimensions?)
    BALANCED: 256		// Flag: element computations are all the same cost
};

// (Not part of the API)
//
// TypedObject array cheat sheet, using June 2014 TypedObject API:
//
//   const Grid = TypedObject.uint32.array(10).array(15);  // array(15) of array(10)
//   const Grid2 = TypedObject.uint32.array(15,10);        // array(15,10)
//   var g = new Grid()
//   var rows = g.length
//   var cols = g[0].length  // iff g.length > 0
//
//   Grid.length == g.length
//   TypedObject.objectType(g) === Grid
