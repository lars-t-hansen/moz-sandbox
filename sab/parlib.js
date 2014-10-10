// Utilities for plain js to use shared memory (relatively)
// conveniently.
//
// DRAFT.
// 10 October 2014 / lhansen@mozilla.com.
//
//
// Initialization.
//
// Both the master worker (or main thread) program and the slave
// workers must include parlib.js.  To initialize the shared heap, do
// this:
//
// (1) On the master, create a shared heap and set it up:
//
//       var myheap = new SharedArrayBuffer(4*1024*1024);   // 4MB
//       SetupSharedHeap(myheap, "master");
//
// (2) Send the shared heap to slaves:
//
//       w.postMessage(myheap, [myheap]);
//
// (3) Each slave receives the heap in its event handler and also sets it
//     up locally:
//
//       function (ev) {
//         var myheap = ev.data;
//         SetupSharedHeap(myheap, "slave");
//       }
//
// Objects can now be allocated in the shared heap and communicated
// among all the workers.
//
//
// Shared variable objects.
//
// There is a global shared variable type called "SharedVar".  A new
// shared variable (which is really a shared object) is created in a
// worker with "new SharedVar.T()" where T is one of the predefined
// types: ref, int32, and float64:
//
//    var v = new SharedVar.float64()
//
// To write a value in a shared variable use put:
//
//    v.put(3.14159)
//
// To read a value from a shared variable (on another worker) use get:
//
//    var w = v.get()
//
// When reading from a SharedVar.ref the constructor for the object
// type in the variable must be passed, so that the object can be
// created properly:
//
//    var r = new SharedVar.ref()
//    r.put(obj)
// ...
//    var w = r.get(SharedArray.int32)
//
// (SharedVar put and get are memory synchronization points.)
//
//
// Bootstrapping communication among workers.
//
// There is a pre-defined SharedVar.ref called "sharedVar0", which can
// be used among workers to communicate setup information.
//
// (1) On the master, before sharing the heap with the slaves, create
//     a shared data structure containing communication instructions
//     and place it in sharedVar0:
//
//       sharedVar0.put(obj)
//
// (2) On the slave, after initializing the shared heap, get those
//     instructions from the shared variable:
//
//       var obj = sharedVar0.get(constructor)
//
//
// Array objects.
//
// Primitive shared memory array types are constructed with the
// SharedArray constructors:
//
// (1) On any worker, create an array of int32 like this:
//
//       var a = new SharedArray.int32(100);
//
//     The type of a in this case is SharedInt32Array, which will
//     benefit from full JIT optimization and can be accessed using
//     standard element access syntax:
//
//       a[0] = 37;
//       a[1] = 42;
//
// (2) Send the array to another worker via a shared variable, for
//     example:
//
//       sharedVar0.put(a);
//     ...
//       var x = sharedVar0.get(SharedArray.int32)
//
// There are also SharedArray.float64 and SharedArray.ref.  The value
// read from a SharedArray.ref with the [] operator will appear to be
// an integer, it must immediately be processed with an appropriate
// constructor.  For example if the value's type is MySharedType then
// do this:
//
//      var x = MySharedType.fromRef(a[10])
//
//   
//
// Lock objects.
//
// There is a predefined "Lock" object type representing a
// shared-memory mutex.  It has simple methods called "lock()" and
// "unlock()", and also a method called "invoke()" that takes a thunk
// and invokes a thunk with the lock held.
//
// Locks are not (at present) recursive.
//
// Lock and unlock operations are memory synchronization points.  (To
// be discussed.)
//
//
// Storage management.
//
// If our favorite programming language had a useful finalization
// mechanism we could do garbage-collected storage in the polyfill.
// Alas it does not.
//
// ***All*** shared objects in the local heap are therefore retained
// until you call "release" on them.
//
// Once all references to an object in the shared heap are gone, that
// object can be garbage collected (SMOP).
//
//
//
// Caveats.
//
// If an object is sent to another worker and back to the originator
// it will usually come back as a separate object (referencing the
// same shared memory, of course).



// NOTE: It must be possible to create shared-memory /types/ before registering the heap,
// since those types will be created as part of the bootstrap process.  So creating a type can't store things
// in shared memory eagerly.  We can store things in shared memory when the first object of a type is created.
// (If we have to)


// There is one global SharedArrayBuffer, and we map a
// SharedInt32Array 'iab' onto this for administration purposes.
//
// The low addresses are allocated as follows:
//
//   0: reserved for application use (bootstrapping, normally)
//   1: iab next-free pointer (variable, update with CAS)
//   2: iab limit pointer (constant, pointer past the end)
//
// The next-free pointer should be kept 16-byte aligned (4 words).
// It is initialized by the system.
//

// A "shared object" is an index range within the array, wrapped
// within a JS object.  The JS object places an interpretation on the
// range of values.  Each shared object type has a constructor, which
// allocates a new object, an export method that returns a low + length
// pair (really, two integers) and an import static method that
// reconstitutes the object from that range.
//
// Hm, that's deeply unsafe.

// Allocation is normally in a local heap (because global allocation
// is expensive).  Deallocation is explicit for now.

// By and large, for things to work out well, *all* non-constant or
// worker-variant information associated with a shared object should
// be stored in shared memory, not on individual local objects.

var _sab;			// SharedArrayBuffer used for the heap
var _iab;			// SharedInt32Array covering the _sab
var _dab;			// SharedFloat64Array covering the _sab

const _uninit = {};		// A cookie used in object construction.

function SetupSharedHeap(sab, initialize) {
    _sab = sab;
    _iab = new SharedInt32Array(sab);
    _dab = new SharedFloat64Array(sab);
    if (initialize) {
	_iab[0] = 0;
	_iab[1] = 16;
	_iab[2] = _iab.length;
    }
}

function _localAlloc(n) {
}

function _localAllocWord() {
}

//////////////////////////////////////////////////////////////////////
//
// 'Struct' type shared data.
//
// Each object maps to some number of words in _iab.
//
// The first word of an object is a descriptor:
//
//   +--+--------------------------+----+
//   | B| F                        | C  |
//   +--+--------------------------+----+
//
// B are two reserved bits.
// C is a field count (four bits, max 13 fields).
// F is a bit vector of field descriptors, two bits per word in the object:
//
//   00   padding / unused
//   01   ref
//   10   float64 (either word)
//   11   int32
//
// F is filled from the lower-order bits toward higher-order.  The object is
// allocated on an even-numbered word boundary.  Float64 data are allocated 
// on an even-numbered word boundary too.
//
// Unused high bits in F must be filled with zeroes.

const _i32_field = 0;		// Descriptor for an int32 field
const _ref_field = 1;		// Descriptor for a shared object reference field
const _f64_field = 2;		// Descriptor for a double field

function _allocObject(nwords, obj, desc) {
    var v = ...;
    var iab = _iab;
    obj._base = v;
    iab[v] = desc;
    for ( int i=1 ; i < nwords ; i++ )
	iab[v+i] = 0;
    return v;
}

// Common code for shared 'struct' data.

// Each type has its own prototype, containing the type's tag (for now).

function SharedObjectProto(tag) {
    this._tag = tag;
    this.initialize = () => throw new Error("No initialize method in " + tag);
}

// All shared struct objects' prototypes share this prototype.

SharedObjectProto.prototype = {
    toString: (() => "Shared " + this._tag), // _tag on the struct objects' prototypes
    toRef: (() => this._base)		     // _base on the object itself
};

// Construct a fromRef function for a type, given its constructor

function SharedObjectFromReffer(constructor) {
    var d = constructor._desc;
    if (!d)
	throw new Error("Bad constructor: no _desc: " + constructor);
    return function (ref) {
	if (_iab[ref] != d)
	    throw new Error("Bad reference: unmatched descriptor: wanted " + d + ", got " _iab[ref]);
	var l = new constructor(_uninit);
	l._base = ref;
	l.initialize(ref);
	return l;
    };
}

//////////////////////////////////////////////////////////////////////
//
// 'Array' type shared data.
//
// Each object maps to some number of words in _iab.
//
// The first word of an object is a descriptor, as for 
//
//   +--+--------------------------+----+
//   | B| F                        | C  |
//   +--+--------------------------+----+

// Monotyped arrays are allocated with a two-word header:
//  - special 'array of t' descriptor
//  - element count


function SharedArrayFromReffer(constructor) {
    var d = constructor._desc;
    if (!d)
	throw new Error("Bad constructor: no _desc: " + constructor);
    return function (ref) {
	if (_iab[ref] != d)
	    throw new Error("Bad reference: unmatched descriptor: wanted " + d + ", got " _iab[ref]);
	var elt = _iab[ref+1];
	var l = new constructor(_uninit);
	l._base = ref;
	return l;
    };
};


//////////////////////////////////////////////////////////////////////
//
// SharedVar objecs.
//

function SharedVar(cookie) {
}

function Mbox0Put(o) {
    Atomics.store(_iab, 0, o ? o.toRef() : 0);
}

function Mbox0Get(constructor) {
    var v = Atomics.load(_iab, 0);
    return v ? constructor.fromRef(v) : null;
}

//////////////////////////////////////////////////////////////////////
//
// Lock objects.
//
// The mutex code is based on http://www.akkadia.org/drepper/futex.pdf.
//
// Mutex state values:
//   0: unlocked
//   1: locked with no waiters
//   2: locked with possible waiters
//
// _iab[index] must be initialized (globally) to 0 before the first
// mutex is created, that's currently done in the allocator.

function Lock(cookie) {
    // Required pattern
    if (cookie !== _uninit) {
	var v = _allocObject(2, this, Lock.desc);
	this.initialize(v);
    }
}

// Required derivation
Lock.prototype = new SharedObjectProto("Lock");

// Required field
Lock._desc = (_i32_field << 4) | 1;

// Required method
Lock.fromRef = SharedObjectFromReffer(Lock);

// Required method
Lock.prototype.initialize =
    function (ref) {
	this.index = ref+1;
    };

// Required method
Lock.prototype.release =
    function () {
	this.index = -1;
	this._base = -1;
	//this._next._prev = this._prev;
	//this._prev._next = this._next;
    };

Lock.prototype.lock = 
    function () {
	const iab = _iab;
	const index = this.index;
	var c;
	if ((c = Atomics.compareExchange(iab, index, 0, 1)) != 0) {
	    do {
		if (c == 2 || Atomics.compareExchange(iab, index, 1, 2) != 0)
		    Atomics.futexWait(iab, index, 2, Number.POSITIVE_INFINITY);
	    } while ((c = Atomics.compareExchange(iab, index, 0, 2)) != 0);
	}
    };

Lock.prototype.unlock =
    function () {
	const iab = _iab;
	const index = this.index;
	var v0 = Atomics.sub(iab, index, 1);
	if (v0 != 1) { // Wake up a waiter if there are any.
	    Atomics.store(iab, index, 0);
	    Atomics.futexWake(iab, index, 1);
	}
    };
	
Lock.prototype.invoke =
    function (thunk) {
	try {
	    l.lock();
	    return thunk();
	}
	finally {
	    l.unlock();
	}
    };

//////////////////////////////////////////////////////////////////////
//
// Primitive arrays.

const SharedArray = {};

SharedArray.int32 =
    function (nelements) {
	var v = _allocWords(2+nelements);
	_iab[v+0] = _array_int32;
	_iab[v+1] = nelements;
	var a = new SharedInt32Array(_sab, 8+(v*4), nelements);
	a._base = v;
	return a;
    };
SharedArray.int32._desc = _array_int32;
SharedInt32Array.fromRef = SharedArrayFromReffer(SharedArray.int32);
SharedInt32Array.prototype.toRef = () => return this._base;

SharedArray.float64 =
    function (nelements) {
	var v = _allocWords(2+nelements*2);
	_iab[v+0] = _array_float64;
	_iab[v+1] = nelements;
	var a = new SharedFloat64Array(_sab, 8+v*8, nelements);
	a._base = v;
	return a;
    };
SharedArray.float64._desc = _array_float64;
SharedFloat64Array.fromRef = SharedArrayFromReffer(SharedArray.float64);
SharedFloat64Array.prototype.toRef = () => return this._base;


/*
// Shared typed objects.
//
// Storage management is essentially manual.  Each JS object that
// fronts a shared object must have a release method that will unlink
// the JS object from a set that keeps the underlying shared object
// alive.  Once all references are gone, the shared memory may be
// reclaimed.



function SharedTypedObject(layout) {
}

SharedTypedObject.StructType =
    function(fields) {
	var loc = 0;
	var desc = "";
	for ( var i in fields ) {
	    if (!fields.hasOwnProperty(i))
		continue;
	    switch (fields[i]) {
	    case SharedTypedObject.int32: 
		desc += `get ${i}: function() { return _iab[this._index + ${loc}] }`;
		loc += 4;
		break;
	    case SharedTypedObject.atomic_int32:
		desc += `get ${i}: function() { return Atomic.load(_iab, this._index + ${loc}) }`;
		loc += 4;
		break;
	    case SharedTypedObject.float64: 
		loc = (loc + 7) & ~7;
		desc += `get ${i}: function() { return _dab[(this._index + ${loc}) >> 1] }`;
		loc += 8;
		break;
	    case SharedTypedObject.ref:
	    case SharedTypedObject.atomic_ref:
		// TBD
		break;
	    default:
		fail();
		break;
	    }
	}
    };

SharedTypedObject.ArrayType =
    function (baseType) {
    };

SharedTypedObject.ForwardType =
    function () {
    };

SharedTypedObject.castReference =
    function (r, type) {
	// Interpret r as the given type.
    };

SharedTypedObject.int32 = {toString:() => "shared int32"};
SharedTypedObject.float64 = {toString:() => "shared float64"};
SharedTypedObject.ref = function (T) { return {toString:() => "shared ref", referent:T} };

SharedTypedObject.atomic_int32 = {toString:() => "shared atomic int32"};
SharedTypedObject.atomic_ref = function(T) { return {toString:() => "shared atomic ref", referent:T} };

// test code

var T = SharedTypedObject;

var _Node = T.ForwardType();
var Node = T.StructType(_Node, {left:T.ref(_Node), right:T.ref(_Node), x:T.int32});

var root = new Node({left:_null, right:_null, x:-1}); // Shared memory, yay!
root.release();					      // Free attachment

*/
