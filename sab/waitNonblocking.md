# Atomics.waitNonblocking (proposal)

Background: this feature has been tossed about in various forms, initially under the name Atomics.waitCallback.

The purpose is to provide an API that the main thread can use to perform waiting on a shared memory location without blocking the main thread.  The API is promise-based; ultra-high-performance is not a requirement, though there is one optimization in particular that is desirable and its viability should be discussed.

## Synopsis

`Atomics.waitNonblocking(i32a, index, value, [timeout]) => result`

Where `i32a` is an Int32Array mapping a SharedArrayBuffer, `index` is a valid index within that array, `value` will be converted to Int32 and compared against the value in the array at that index, and `timeout`, if present, is a timeout value interpreted as for `Atomics.wait`.

The `result` is a promise.  The promise is resolved with a string value: "ok", "timed-out", "not-equal".

Notably other workers can call `Atomics.wake` on the location to wake anyone waiting with `waitNonblocking`.  The waker does not need to know how the waiter is waiting.

The functionality is meant to mimic `Atomics.wait`.  There's no reason in particular why the API can't be used off the main thread, and it may be desirable to do so in some cases.

## Polyfills

It should be possible to polyfill this at some cost and inconvenience.  The polyfill needs to wrap Atomics.wake, and there needs to be some shared state for the polyfill to use.

## Implementation challenges

TBD 

## Performance

One optimization in particular is desirable: if the value in the array does not match the expected value at the time of the call, or if the waiter goes to sleep but is quickly woken, it would be best if the promise could be resolved quickly, ideally without going through a heavyweight job scheduling mechanism.  But I don't know (yet) if it's the done thing to resolve the promise synchronously.
