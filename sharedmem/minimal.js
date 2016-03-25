// A completely minimal shared-memory API

function Shared(nbytes) {
    this._mem = new SharedArrayBuffer(nbytes);
    this._i32 = new Int32Array(this._mem);
    this._i8 = new Int8Array(this._mem);
    this._f64 = new Float64Array(this._mem);
}

// "SC" = sequentially consistent
// "AR" = acquire-release, if we had it
// "REL" = relaxed, if we had it

// We could use "at" and "put" for load and store and drop the SC
// for the seqCst ops.

Shared.int32 = {
    load: (s, x) => s._i32[x],
    store: (s, x, v) => s._i32[x] = v,
    loadSC: (s, x) => Atomics.load(s._i32, x),
    storeSC: (s, x, v) => Atomics.store(s._i32, x, v),
    addSC: (s, x, v) => Atomics.add(s._i32, x, v),
    subSC: (s, x, v) => Atomics.sub(s._i32, x, v),
    andSC: (s, x, v) => Atomics.and(s._i32, x, v),
    orSC: (s, x, v) => Atomics.or(s._i32, x, v),
    xorSC: (s, x, v) => Atomics.xor(s._i32, x, v),
    exchangeSC: (s, x, v) => Atomics.exchange(s._i32, x, v),
    compareExchangeSC: (s, x, o, n) => Atomics.compareExchange(s._i32, x, o, n),
    isLockFree: () => Atomics.isLockFree(4),
    wait: (s, x, v, t) => Atomics.futexWait(s._i32, x, v, t),
    wake: (s, x, n) => Atomics.futexWake(s._i32, x, n),
    TYPE: "int32",
    BYTE_SIZE: 4
}

Shared.int8 = {
    load: (s, x) => s._i8[x],
    store: (s, x, v) => s._i8[x] = v,
    loadSC: (s, x) => Atomics.load(s._i8, x),
    storeSC: (s, x, v) => Atomics.store(s._i8, x, v),
    // etc
    compareExchangeSC: (s, x, o, n) => Atomics.compareExchange(s._i8, x, o, n),
    isLockFree: () => Atomics.isLockFree(1),
    TYPE: "int8",
    BYTE_SIZE: 1
}

Shared.float64 = {
    load: (s, x) => s._f64[x],
    store: (s, x, v) => s._f64[x] = v,
    // that's all you get
    TYPE: "float64",
    BYTE_SIZE: 8
}

Shared.int32x4 = {
    load: (s, x) => SIMD.int32x4.load(s._i32, x),
    store: (s, x, v) => SIMD.int32x4.store(s._i32, x, v),
    TYPE: "int32x4",
    BYTE_SIZE: 16
};

// This is a minimal view type, for the use of DOM primarily, so that
// a Shared slice can be packaged and passed around conveniently to
// existing DOM APIs.
//
// This can be provided by DOM, not ECMAScript.
//
// The type is a string denoting the type, one of the Shared.T.TYPE
// values.  The offset and length are offsets within the memory in
// terms of that type.
//
// The views have few methods on purpose.

function SharedView(s, type, sizeOffs, sizeLen) {
    // TODO: check s is a Shared
    this._mem = s;
    let size = 0;
    switch (type) {
    case "int8": size = 1; break;
    case "int32": size = 4; break;
    case "float64": size = 8; break;
    default: throw new Error("Bad type: " + type);
    }
    // TODO: range checking
    this._byteOffset = size*sizeOffs;
    this._byteLength = size*sizeLen;
    this._size = size;
}

SharedView.prototype.defineProperty({"buffer": { "get": () => this._mem }});
SharedView.prototype.defineProperty({"byteOffset": { "get": () => this._byteOffset }});
SharedView.prototype.defineProperty({"length": { "get": () => this._byteOffset }});

/// How to use

// Aliases are handy

let I32 = Shared.int32;

// Double every element in an int array

function double(sm, start, limit) {
    for ( let i=start ; i < limit ; i++ )
	I32.store(sm, i, I32.load(sm, i)*2);
}

// Or even:

let li32 = Shared.int32.load;
let si32 = Shared.int32.store;

function double(sm, start, limit) {
    for ( let i=start ; i < limit ; i++ )
	si32(sm, i, li32(sm, i)*2);
}

// (Notably this lets names be abbreviated, unlike(?) methods)

// Anyway contrast with this:

function double_old(ia, len) {
    for ( let i=0 ; i < len ; i++ )
	ia[i] *= 2;
}

// Double every element in any array (no abbreviation possible)

function double(sm, ops, start, limit) {
    for ( let i=start ; i < limit ; i++ )
	ops.store(sm, i, ops.load(sm, i)*2);
}
