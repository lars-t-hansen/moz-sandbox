# Atomics.waitNonblocking (preliminary proposal)

Author: Lars T Hansen (lhansen@mozilla.com) / April 20, 2017

We provide a new API, `Atomics.waitNonblocking`, that an agent can use
to wait on a shared memory location without blocking.  Notably this
API is useful in agents whose [[CanBlock]] attribute is `false`, such
as the main thread of a browser document, but the API is not
restricted to such agents.

The API is promise-based.  Very high performance is *not* a
requirement, but good performance is.

Prior history: This API has been proposed several times before, indeed
it was in early drafts of the shared memory proposal under the name
`Atomics.futexWaitCallback`.

Rationale: This API was not included in ES2017 so as to simplify the
initial API of shared memory and atomics, but it is a desirable API
for smooth integration of shared memory into the idiom of ECMAScript.
A simple polyfill is possible but a native implementation will likely
have much better performance than the polyfill.


## Synopsis

`Atomics.waitNonblocking(i32a, index, value, [timeout]) => result`

The arguments are intepreted and checked as for `Atomics.wait`.  If
argument checking fails an exception is thrown synchronously, as for
`Atomics.wait`.

* `i32a` is an Int32Array mapping a SharedArrayBuffer
* `index` is a valid index within `i32a`
* `value` will be converted to Int32 and compared against the contents of `i32a[index]`
* `timeout`, if present, is a timeout value

The `result` is a promise.  The promise can be resolved with a string
value, one of "ok", "timed-out", "not-equal"; the value has the same
meaning as for the return type of `Atomics.wait`.  The promise is
never rejected.

Agents can call `Atomics.wake` on some location corresponding to
`i32a[index]` to wake any agent waiting with
`Atomics.waitNonblocking`.  The agent performing the wake does not
need to know how that waiter is waiting with `waitNonblocking`.


## Informal semantics (aka notable facts)

Multiple agents can `waitNonblocking` on the same location at the same
time.  A `wake` on the location will resolve all the waiters'
promises, in some unspecified interleaving with unspecified
concurrency.

A single agent can `waitNonblocking` multiple times on a single
location before any of the waits are resolved.  A `wake` on the location
will resolve (in some arbitrary non-concurrent sequence) all the
promises.

Some agents can `wait` and other agents can `waitNonblocking` on the
same location at the same time, and a `wake` will wake all waiters
regardless of how they are waiting.

A single agent can first `waitNonblocking` and then, before that wait
is resolved, `wait` on a location.  A `wake` on the location will
first wake the agent from the second `wait`, and then resolve the
promise.

A single agent can `waitNonblocking` on a location and can then `wake`
on that location to resolve that promise within itself.

More generally, an agent can `waitNonblocking` on a location and only
subsequent to that take action that will cause some agent to perform a
`wake`.  For this reason, an implementation of `waitNonblocking` that
blocks is not viable (though see the Performance section).

For practical purposes, we can think of the semantics as being those
of an implementation that creates a new helper agent for each
`waitNonblocking` call; this helper agent performs a normal blocking
`wait` on the location; and when awoken, it sends a signal to the
originating agent to resolve the promise with the appropriate result
value.


## Open questions

In the case where a single agent creates several nonblocking waits on
the same location, an argument could be made that they should be
resolved in order.  (The current shared memory spec stipulates that
waiters are awoken in wait order generally, and this is observable.)
If this falls out of how promises are specified, then that's great,
otherwise we should investigate what we can do.


## Polyfills

A simple polyfill is possible.  As suggested by the semantics, in the
Web domain it uses a helper Worker that performs a blocking `wait` on
behalf of the agent that is performing the `waitNonblocking`; that
agent and the helper communicate by message passing to coordinate
waiting and promise resolution.

As Workers are heavyweight and message passing is relatively slow, the
polyfill does not have excellent performance, but it is a reasonable
implementation and has good fidelity with the semantics.  (Helpers are
reused when possible.)

The polyfill will not work in agents that cannot create new Worker
objects, either if they are too limited (worklets?) or if nested
Workers are not allowed (some browsers) or if a Worker cannot be
created from a data: URL.

See `waitNonblocking.js` in this directory for the polyfill and
`waitNonblocking.html` for some test cases.


## Implementation challenges

TBD.


## Performance and optimizations

For performance reasons it might appear that it is desirable to
"resolve the promise synchronously" if possible.  Leaving aside what
that would mean, here are some cases where the result of the
`waitNonblocking` could be available directly:

* The value in the array does not match the expected value and we can
  resolve synchronously with "not-equal"
* The value in the array matches and we have to sleep, but we want to
  micro-wait to see if a wakeup is received soon, in which case we can
  resolve synchronously with "ok"
* The value in the array matches but the timeout is zero, in which
  case we can resolve synchronously with "timed-out"

The first case is probably somewhat important.

The second case can backfire if the waiting agent takes action to
ensure the wakeup only after creating the waiting promise.  But absent
that the case can be important for the performance of
producer-consumer problems.

The third case is not important; it is just a mystification of
`Atomics.load`.

Note that we can never resolve synchronously with "timed-out" if the
timeout is nonzero because we don't know if the waiting agent is going
to take action to perform the wakeup after setting up the wait.

However, synchronous resolution is probably not really viable.  In
practice, we'd want `waitNonblocking` to return either a string result
or a promise, and for the calling code to act on the type of the
return value.  This is messy, and it plays poorly with `await`.

I think instead that when performance matters to that degree, the
first case can be handled with an explicit check preceding
`waitNonblocking`, and in addition the implementation can create a
promise that resolves directly without involving any actual waiting.
For the second case we should resuscitate the old idea of
`Atomics.pause`, which allows for controlled micro-waits in agents
that otherwise cannot block.
