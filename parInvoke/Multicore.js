// Polyfill for the Multicore object
//
// 2014-06-20 / lhansen@mozilla.com
//

// Multicore.build()
//   Invokes its function argument in parallel across multiple worker
//   threads, producing values into an existing output volume.  It's
//   essentially a nested loop over a subspace of the output volume
//   where all iterations are independent.
//
// Multicore.splitDimension()
//   Computes a reasonable number of slices when given a dimension
//   size and an estimate of the expense of the computation.  It is
//   useful when a computation needs to allocate intermediate
//   storage (as eg for a reduction).


// Multicore.build()
//
// Signature:
//   Multicore.build(fn, obj, iterSpace, origin, hint) => newobj
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
//   'iterSpace' must be an array whose length I is at least 1 and no
//     greater than R, consisting entirely of arrays.  The subarrays
//     must be of length 2, 3, or 4.  The two first elements must be
//     integers, the first no greater than the second.  If there is a
//     third element it must be the value of Multicore.SPLIT.  If a
//     subarray has such a SPLIT directive, then all subarrays
//     preceding it must have the directive too.  The fourth element,
//     if present, is the desired slice size (in number of elements)
//     along that direction.
//
//   'origin' must be either undefined or an array of length R, whose
//     elements must all be nonnegative integers.  If undefined it is
//     interpreted as [0,...,0].
//
//     The two first values of the inner arrays of iterSpace define
//     the logical bounds for iteration across each dimension
//     (outermost dimension leftmost in iterSpace).  These are added
//     to values from 'origin' to produce the storage bounds B_D for
//     each dimension D.  We require 0 <= B_D < L_D for all D.
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
//
// The iteration is performed as follows.
//
// If no dimensions are split, then:
//
// * If I is equal to R, then fn is passed I values representing the
//   index of the element that is to be computed and must return the
//   value that will be stored in newobj.
//
// * If I is less than R, then fn is passed I values representing the
//   smallest (think top-left-nearest) index of the subvolume that is
//   to be computed as well as an origin-zero array representing that
//   subvolume; the rank of this array is R-I.  Fn can freely read and
//   write this array at any location; initial values are "reasonable
//   defaults" (zero for numbers, null for pointers, and undefined for
//   Any and in Arrays).  Fn need not write all values in the array.
//   The return value of fn is ignored.
//
// If any dimensions are split, then:
//
// * Iteration will be performed as if R = 2I in the second case
//   above, that is, the iteration proceeds over rank-R subvolumes
//   whose sizes are computed according to the parameters of the SPLIT
//   directives, the hint parameter, and internal defaults.
//
//   If outer dimensions are SPLIT but inner dimensions are not then
//   the subvolumes will cover entire index ranges in the unsplit
//   dimensions.
//
//   For example, consider the 2D case.  If both dimensions are SPLIT
//   then fn will be passed "tiles" comprising several partial rows.
//   If only the outer dimension is SPLIT then fn will be passed
//   "strips" comprising several complete rows.
//
// In all cases, invocations of fn are independent and no invocation
// of fn can observe values written by other invocations.
//
//
// Issues:
// - While it is plausible to "neuter" an Array it's not certain that
//   it's plausible (in the implementation) to pass in a pointer that
//   references only part of that Array's storage.  So it may be that
//   if obj is an Array we require origin to be [0], or there will
//   have to be some copy-in/copy-out overhead.
//
// Notes:
// - In iterSpace, the integer values can be negative, so long
//   as the origin accounts for that (the range check is the final
//   arbiter).
// - The hint suggests the cost of each element computation, and
//   can in a pinch be used to direct the scheduler.  Mostly
//   it should be left unspecified.  Other hints may be introduced
//   later.
//
// Performance issues to resolve:
//
// - Multicore.build should be cognizant of higher-level attempts at
//   work distribution.  For example, if a dimension is split then the
//   splitting should match scheduling.  For another example, it
//   should be possible to ask how splitting will be performed so that
//   a temporary data structure of appropriate size can be allocated.
//
// - Probably won't need to expose Par.numWorkers() in order to allow
//   the primitive to be used well for reduction-like tasks, it is
//   better to talk about "how splitting will be done".


// TypedObject array cheat sheet, using June 2014 TypedObject API:
//
//   const Grid = TypedObject.uint32.array(10).array(15);  // array
//   const Grid2 = TypedObject.uint32.array(15,10);        // 15x10 array
//   var g = new Grid()
//   var rows = g.length
//   var cols = g[0].length  // iff g.length > 0
//
//   Grid.length == g.length
//   TypedObject.objectType(g) === Grid


function Multicore_build(fn, obj, iterSpace, origin, hint)
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
    const DEFAULT = 1;         // Value of Multicore.DEFAULT
    const COARSE = 2;          // Value of Multicore.COARSE
    const FINE = 4;	       // Value of Multicore.FINE
    const BALANCED = 256;
    const FLAGMASK = ~255;

    const PRETEND_TO_NEUTER = true; // More performant, OK for now

    var R=0, I=0, isArray=false, isTypedObj=false, coarse=false, splitCount=0, splits=null, defaultSplitSize=0;

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
	ThrowError(JSMSG_BAD_TYPE, obj);

    if (!IsTrueArray(iterSpace))
	ThrowError(JSMSG_BAD_TYPE, iterSpace);

    // Checking hint before iterSpace because we will use the hint
    if (hint === undefined)
	hint = DEFAULT;
    // TODO: Want to randomize the defaults a little to avoid code depending on them?
    switch (hint & ~FLAGMASK) {
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

    I = iterSpace.length;
    if (I < 1 || I > R)
	ThrowError(JSMSG_BAD_ARRAY_LENGTH);

    for (var i=0; i < I; i++) {
	var v = iterSpace[i];
	if (!IsTrueArray(v))
	    ThrowError(JSMSG_BAD_TYPE, v);
	if (v.length < 2 || v.length > 4)
	    ThrowError(JSMSG_BAD_ARRAY_LENGTH);
	if (!IsInt32(v[0]) || !IsInt32(v[1]) || v[0] > v[1])
	    ThrowError(JSMSG_BAD_VALUE, v);
	if (v.length >= 3) {
	    if (v[2] == SPLIT) {
		if (!splits)
		    splits = [];
		if (splitCount < i)
		    ThrowError(JSMSG_BAD_TYPE, v);
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

    if (origin === undefined) {
	origin = [];
	for (var i=0; i < R; i++)
	    origin.push(0);
    }
    else {
	if (!IsTrueArray(origin))
	    ThrowError(JSMSG_BAD_TYPE, origin);
	if (origin.length != R)
	    ThrowError(JSMSG_BAD_ARRAY_LENGTH);
	for (var i=0; i < R; i++ ) {
	    if (!IsInt32(origin[i]) || origin[i] < 0)
		ThrowError(JSMSG_BAD_TYPE, origin);
	}
    }

    if (isArray && !IsPackedArray(obj))
	MakePackedArray(obj);

    var result = NeuterObjectAndClone(obj);

    // There are several dimensions for specialization here:
    //
    //  - I == R vs I < R (called "exact" vs "inexact" at present)
    //  - Array vs TypedObject array vs other types (if supported at all)
    //  - Specialized values of I (usually 1..3 or 1..7) and unspecialized
    //  - Sequential vs parallel
    //  - Type specialization / cloning

    switch (I) {
    case 1:
	var lo=iterSpace[0][0]+origin[0];
	var hi=iterSpace[0][1]+origin[0];
	if (lo < 0 || hi > result.length)
	    ThrowError(JSMSG_BAD_INDEX);
	return splitCount ? exact1split(lo, hi) : exact1(lo, hi);
    case 2:
	var lo0=iterSpace[0][0]+origin[0];
	var hi0=iterSpace[0][1]+origin[0];
	var lo1=iterSpace[1][0]+origin[1];
	var hi1=iterSpace[1][1]+origin[1];
	if (lo0 < 0 || hi0 > result.length ||
	    lo1 < 0 || hi1 > DimLength(result, 1))
	{
	    ThrowError(JSMSG_BAD_INDEX);
	}
	if (R == I)
	    return splitCount ? exact2split(lo0, hi0, lo1, hi1) : exact2(lo0, hi0, lo1, hi1);
	return splitCount ? inexact2split(lo0, hi0, lo1, hi1) : inexact2(lo0, hi0, lo1, hi1);
    case 3:
	var lo0=iterSpace[0][0]+origin[0];
	var hi0=iterSpace[0][1]+origin[0];
	var lo1=iterSpace[1][0]+origin[1];
	var hi1=iterSpace[1][1]+origin[1];
	var lo2=iterSpace[2][0]+origin[2];
	var hi2=iterSpace[2][1]+origin[2];
	if (lo0 < 0 || hi0 > result.length ||
	    lo1 < 0 || hi1 > DimLength(result, 1) ||
	    lo2 < 0 || hi2 > DimLength(result, 2))
	{
	    ThrowError(JSMSG_BAD_INDEX);
	}
	if (R == I)
	    return splitCount ? exact3split(lo0, hi0, lo1, hi1, lo2, hi2) : exact3(lo0, hi0, lo1, hi1, lo2, hi2);
	return splitCount ? inexact3split(lo0, hi0, lo1, hi1, lo2, hi2) : inexact3(lo0, hi0, lo1, hi1, lo2, hi2);
    default:
	throw new Error("Internal error: only up to 3 dimensions supported");
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
	    // Ditto here, for now - but want to do better soon with a shared-subarray idea,
	    // or even better, a cursor like the one the builtins are using.  (There's nothing
	    // inherently bad about a cursor provided it can be neutered.)
	    var slice = TypedObject.objectType(result).elementType.array(sliceSize);
	    for (var id=0 ; id < sliceCount ; id++ ) {
		var baseIndex = lo + id*sliceSize;
		var len = sliceSize;
		if (id == sliceCount-1) {
		    len = lastSize;
		    slice = TypedObject.objectType(result).elementType.array(lastSize); // Ouch!!
		}
		for ( var i=0 ; i < len ; i++ )
		    slice[i] = 0; // 0 is really only right for numeric types
		fn(baseIndex, slice);
		for ( var i=0 ; i < len ; i++ )
		    result[baseIndex+i] = slice[i];
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
	// Splitting can happen along one or two dimensions; we end up with strips and tiles, respectively.
	var range0 = hi0-lo0;
	var range1 = hi1-lo1;
	if (range0 == 0 || range1 == 0)
	    return result;

	var sliceSize0 = splits[0];
	var sliceCount0 = Math.ceil(range0/sliceSize0);
	var lastSize0 = range0 % sliceSize0;
	if (lastSize0 == 0)
	    lastSize0 = sliceSize0;

	// If we split along the first dimension but not the second, then we have two choices:
	// produce tiles, or produce strips.  Here I choose to produce strips, since tiles
	// can be produced by actually specifying splitting along the second dimension.

	var sliceSize1 = splitCount < 2 ? range1 : splits[1];
	var sliceCount1 = Math.ceil(range1/sliceSize1);
	var lastSize1 = range1 % sliceSize1;
	if (lastSize1 == 0)
	    lastSize1 = sliceSize1;

	// Again, copy-out for the polyfill.  This must be fixed, as for the 1D case.
	// We count on creating a subarray of the result that references a subgrid
	// as if it were a 2D array.  This means slightly more complicated code in
	// the generated indexing expression since a different row length is used for
	// range checking and offset computation.

	var T = TypedObject.objectType(result).elementType.elementType;
	var slice = new (T.array(sliceSize0,sliceSize1));
	for (var id0=0 ; id0 < sliceCount0 ; id0++ ) {
	    for ( var id1=0 ; id1 < sliceCount1 ; id1++ ) {
		var baseIndex0 = lo0 + id0*sliceSize0;
		var len0 = sliceSize0;
		if (id0 == sliceCount0-1)
		    len0 = lastSize0;

		var baseIndex1 = lo1 + id1*sliceSize1;
		var len1 = sliceSize1;
		if (id1 == sliceCount1-1)
		    len1 = lastSize1;

		if (len0 != sliceSize0 || len1 != sliceSize1)
		    slice = new (T.array(len0,len1));

		for ( var i=0 ; i < len0 ; i++ )
		    for ( var j=0 ; j < len1 ; j++ )
			slice[i][j] = 0;
		fn(baseIndex0, baseIndex1, slice);
		for ( var i=0 ; i < len0 ; i++ )
		    for ( var j=0 ; j < len1 ; j++ )
			result[baseIndex0+i][baseIndex1+j] = slice[i][j];
	    }
	}

	return result;
    }

    function exact3(lo0, hi0, lo1, hi1, lo2, hi2) {
	for (; lo0 < hi0; lo0++)
	    for (var i=lo1; i < hi1; i++)
		for (var j=lo2; j < hi2; j++)
		    result[lo0][i][j] = fn(lo0, i, j);
	return result;
    }

    function inexact() {
	return result;		// FIXME
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

// Multicore.splitDimension()
//
// Signature:
//   Multicore.splitDimension(size, hint) => object
//
// Arguments:
//   'size' is the size of the dimension being split.  Size must be
//     positive.
//
//   'hint' is a hint about the expense of the computation for each
//     element: Multicore.FINE, Multicore.COARSE, or Multicore.DEFAULT;
//     for all the Multicore.BALANCED flag may be or'ed in.
//
// Return value:
//   Returns an object with these properties:
//     - 'numSlices' is the total number of slices, numSlices > 0
//     - 'sliceSize' is the size of all but the last slice, sliceSize > 0
//     - 'lastSize' is the size of the last slice, lastSize > 0
//
// The logic here is more or less as follows:
//
// - The default is some variation on Shu's algorithm, for example
// - If computations are coarse the slice size is smaller
// - If computations are fine the slice size is larger
// - If computations are unbalanced then the slice size is smaller
// - If computations are balanced the slice size is larger
//
// "Balanced, fine" slices should be some small constant times the
// number of cores.
//
// "Unbalanced, coarse" slices should be not much larger than 1.

// Old notes

// For k > 0 returns {numSlices, sliceSize, lastSize} such that numSlices*sliceSize + lastSize == k && 0 <= lastSize <= sliceSize
//
// This is appropriate for slices where the element function is not very expensive.
//
// This turns out to be super useful: it is the go-to function for
// computing slices for intermediate results, where the function just
// needs to distribute work and we expect the slices to be about
// equally expensive.

function Multicore_splitDimension(size, hint)
{
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

var Multicore = {
    build: Multicore_build,
    splitDimension: Multicore_splitDimension,

    // Directives pertaining to how to process a dimension
    // within Multicore.build()
    SPLIT: -1,

    // Directives pertaining to how to create a schedule
    // for Multicore.build() or Multicore.splitDimension()
    DEFAULT: 1,	                // Element computations are of unknown cost
    COARSE: 2,			// Element computations are expensive
    FINE: 4,			// Element computations are cheap
    BALANCED: 256		// Flag: element computations are all the same cost
};
