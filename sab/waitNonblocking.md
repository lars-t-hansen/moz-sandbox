# Atomics.waitNonblocking (proposal)

Background: this feature has been tossed about in various forms, initially under the name Atomics.waitCallback.

The purpose is to provide an API that the main thread can use to perform waiting on a shared memory location without blocking the main thread.

## Synopsis

`Atomics.waitNonblocking(i32a, index, value, [timeout]) => result`

Where `i32a` is an Int32Array mapping a SharedArrayBuffer, `index` is a valid index within that array, `value` will be converted to Int32 and compared against the value in the array at that index, and `timeout`, if present, is a timeout value interpreted as for `Atomics.wait`.

The `result` is a promise.  The promise is resolved with a string value: "ok", "timed-out", "not-equal".

The functionality is meant to mimic `Atomics.wait`.
