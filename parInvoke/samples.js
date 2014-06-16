function Array_static_build(k, fn) {
    return Par.invoke(fn, new Array(k), [[0,k]]);
}

function Array_map(fn) {
    var self = this;
    var k = self.length;
    return Par.invoke(function (i) { return fn(self[i], i, self); }, new Array(k), [[0,k]]);
}

// For k > 0 returns {numSlices, sliceSize, lastSize} such that numSlices*sliceSize + lastSize == k && 0 <= lastSize <= sliceSize
// This is appropriate for slices where the element function is not very expensive.
//
// This turns out to be super useful: it is the go-to function for
// computing slices for intermediate results, where the function just
// needs to distribute work and we expect the slices to be about
// equally expensive.
//
// We should formalize this somehow as part of the API.
//
// Also probably formalize something similar for expensive element
// functions.

function Util_computeSlices(k) {
    if (k == 1) {
	//print("Slices: 1 1 0");
	return {numSlices:1, sliceSize:1, lastSize:0};
    }

    var n = Math.max(1, Math.floor(Math.log2(k)));
    var sz = Math.max(1, Math.floor(k / n));
    if (n*sz > k) {
	if (n > 1)
	    n--;
	else
	    sz--;
    }
    if (n <= 0 || sz <= 0)
	throw new Error("Bogus slices");
    var last = k - sz*n;
    if (last > 0)
	n++;
    //print("Slices: " + n + " " + sz + " " + last);
    return {numSlices:n, sliceSize:sz, lastSize:last};
}

function Array_reduce(reducer) {
    var self = this;
    var k = self.length;
    if (k == 0)
	throw new Error("Reduce of array of zero elements");
    if (k == 1)
	return self[0];

    var {numSlices, sliceSize} = Util_computeSlices(k);
    var slices = Par.invoke(reduceSelf, new Array(numSlices), [[0,numSlices]]);
    return reduce2(slices, 0, numSlices);

    // Kernel
    function reduceSelf(sliceId) {
	var lo=sliceId * sliceSize;
	var hi=(sliceId == numSlices-1 ? k : lo+sliceSize);
	return reduce2(self, lo, hi);
    }

    // Kernel
    function reduce2(a, lo, hi) {
	var acc = a[lo];
	for (lo++; lo < hi; lo++ )
	    acc = reducer(acc, a[lo]);
	return acc;
    }
}

// Combining map and reduce is easy:

function Array_mapreduce(mapper, reducer) {
    var self = this;
    var k = self.length;
    if (k == 0)
	throw new Error("MapReduce of array of zero elements");
    if (k == 1)
	return mapper(self[0]);

    var {numSlices, sliceSize} = Util_computeSlices(k);
    var slices = Par.invoke(reduceSelf, new Array(numSlices), [[0,numSlices]]);
    var acc = slices[0];
    for (var i=1; i < numSlices; i++ )
	acc = reducer(acc, slices[i]);
    return acc;

    // Kernel
    function reduceSelf(sliceId) {
	var lo=sliceId * sliceSize;
	var hi=(sliceId == numSlices-1 ? k : lo+sliceSize);
	var acc = mapper(self[lo]);
	for (lo++; lo < hi; lo++ )
	    acc = reducer(acc, mapper(self[lo]));
	return acc;
    }
}

// Inclusive scan
function Array_scan(fn) {
    // To optimize this, select a sliceSize that is a power of 2, to avoid division in gatherSlices
    var self = this;
    var k = self.length;
    var {numSlices, sliceSize} = Util_computeSlices(k);
    var slices = Par.invoke(scanSlice, new Array(numSlices), [[0,numSlices]]); // array of arrays
    var intermediate = new Array(numSlices+1); // array of values
    intermediate[0] = 0;
    for (var i=0; i < numSlices; i++)
	intermediate[i+1] = finalValueOfSlice(i) + intermediate[i];
    return Par.invoke(gatherSlices, new Array(k), [[0, k]]);

    function finalValueOfSlice(i) {
	var x = slices[i];
	return x[x.length-1];
    }

    // Kernel
    function scanSlice(sliceId) {
	var lo=sliceId * sliceSize;
	var hi=(sliceId == numSlices-1 ? k : lo+sliceSize);
	var acc = new Array(hi-lo);
	var i=0, lim=hi-lo;
	acc[0] = self[lo+i];
	for (++i; i < lim; i++)
	    acc[i] = fn(acc[i-1], self[lo+i]);
	return acc;
    }

    // Kernel
    function gatherSlices(i) {
	var sliceId = Math.floor(i / sliceSize); // Javascript stinks, integer division should be a primitive operation
	return slices[sliceId][i % sliceSize] + intermediate[sliceId];
    }
}

function Array_filter(fn) {
    var self = this;
    var k = self.length;
    var {numSlices, sliceSize} = Util_computeSlices(k);
    var buckets = Par.invoke(collect, new Array(numSlices), [[0, numSlices]]);
    // The map is probably superflous, the length extraction can be performed by the scan kernel
    var uptoPos = Array_scan.call(Array_map.call(buckets, (x) => x.length), (a,b) => a+b);
    var num = uptoPos[uptoPos.length-1];
    return Par.invoke(distribute, new Array(num), [[0, num]]);

    // Kernel
    function collect(sliceId) {
	var lo=sliceId * sliceSize;
	var hi=(sliceId == numSlices-1 ? k : lo+sliceSize);
	var result = [];
	for (; lo < hi; lo++) {
	    var v = self[lo];
	    if (fn(v))
		result[result.length] = v; // Array.push would have been nice
	}
	return result;
    }

    // Kernel
    function distribute(i) {
	// Imagine a binary search for j as a better alternative.
	var j=0;
	while (i >= uptoPos[j])
	    j++;
	return buckets[j][j > 0 ? i-uptoPos[j-1] : i];
    }
}

// Remember: when targets[i] == k then result[k] == input[i].

function Array_scatter(targets, defaultValue, conflictFunc, length) {
    var self = this;
    var k = this.length;
    var resultLength = length === undefined ? k : length;
    var {numSlices, sliceSize} = Util_computeSlices(k);
    var t = targets.length;

    if (conflictFunc)
	throw new Error("Conflict function not supported yet");

    var slices = Par.invoke(createMapping, new Array(numSlices), [[0,numSlices]]);
    for ( var i=0 ; i < slices.length ; i++ ) {
	if (!slices[i])
	    throw new Error("Conflict");
    }

    return Par.invoke(computeResult, new Array(resultLength), [[0,resultLength]]);

    // This one trades space for time by having each worker scan the
    // entire array.  The alternative is to have one array the length
    // of the result for each worker, and use a second parallel
    // section to merge them (can be joined with the final copy-in).

    // Kernel
    function createMapping(sliceId) {
	var lo=sliceId * sliceSize;
	var hi=(sliceId == numSlices-1 ? k : lo+sliceSize);
	var result = [];
	result.length = hi-lo;	// new Array() not yet supported in kernels
	for ( var i=0 ; i < t ; i++ ) {
	    if (i in targets) {
		var v = targets[i];
		if (v >= lo && v < hi) {
		    var x = v-lo;
		    if (x in result)
			return null; // Conflict: two target elements target the same result slot
		    result[x] = i;
		}
	    }
	}
	return result;
    }

    // Kernel
    function computeResult(idx) {
	var sliceId = Math.floor(idx / sliceSize);
	var lookup = slices[sliceId];
	var offs = idx - sliceId * sliceSize;
	if (offs in lookup)
	    return self[lookup[offs]];
	return defaultValue;
    }
}

// Presumably there is a generalization of this that zips more than two arrays

function Array_static_zip(a1, a2, combiner) {
    var l1 = a1.length;
    var l2 = a2.length;
    return Par.invoke(
	function (i) {
	    if (i < l1 && i < l2)
		return combiner(a1[i], a2[i]);
	    return i < l1 : a1[i] : a2[i];
	},
	new Array(Math.max(l1, l2)),
	[[0, Math.max(l1, l2)]]);
}

// unzip depends on a simple generalization to Par.invoke that accepts
// and returns multiple output arrays.

function Array_unzip(n, ...fieldGetters) {
    var self = this;
    var k = self.length;
    var as = [];
    var n = fieldGetters.length;
    // n output arrays
    for ( var i=0 ; i < n ; i++ )
	as[i] = new Array(k);
    // Par.invoke2() returns the object 'as' with its properties
    // 0..n-1 updated to hold the result arrays.  as.length will
    // indicate the number of result arrays.  The arrays can be 
    // different lengths, but the iterSpace+origin must be
    // within range of all of them.
    return Par.invoke2(
	function (idx, box) {
	    // Use box properties 0..n-1 to return values for the arrays
	    // The default value is null, and if no value is set then
	    // that value is stored in the output.
	    for ( int i=0 ; i < n ; i++ )
		box[i] = fieldGetters[i](self[idx]);
	},
	as,
	[[0,k]]);
}
