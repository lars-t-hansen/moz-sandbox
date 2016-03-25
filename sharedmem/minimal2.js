// Suppose we really don't want to expose ES to shared memory, and
// we're willing to ignore that programs can just call wasm code to
// access its shared memory.

// What is the minimal amount of stuff we need in ES to still be able
// to write support code, without exposing ES (directly) to raciness?

// Probably ES can't be able to access the racy memory
// (realistically).  This means either accessing it when all workers
// are blocked (not plausible, they will be running custom event
// loops) or having some sort of handle to it that allows ES to talk
// about it but only to pass it around eg to DOM.

// There is a type SharedArrayBuffer, but there is no way to map a
// TypedArray on it.  We can talk about offsets within it and its
// length but not access it.  Access is limited to wasm.

// Now wasm functions can be written to copy-in/copy-out maybe.  They
// will either be racy or will have to coordinate with the workers,
// but that may be OK.

// Basically this will push most runtime code out of JS and into wasm,
// and it'll put pressure on JS-to-wasm calls to be fast, OR we'll
// just export wasm functions a la peek() and poke().

// The only way that will be "fixed" is if:
//
// - wasm can't call ES on the main thread (meaningless if there's wasm there)
// - wasm on the main thread can't access shared memory (weird, since it's a flat heap and there's only one)
// - there is no wasm on the main thread (bizarre but workable I guess)
// - wasm does not use shared memory, ie using a shared heap pushes wasm off the main thread entirely

// Not that TC39 will have any say in the matter any longer, wasm will
// be a box with an API.

// Pushing shared memory off the main thread will push more web APIs
// into workers, for sure.
