// Primitive coroutine interface, provided by the system.
//
// The event thread can create coroutines and transfer control to
// them, and those coroutines can create further coroutines and
// transfer control among themselves.  Control can be transfered back
// to the event thread, which is designated the 'null' coroutine,
// which can then transfer control to other coroutines and so on.
// 
// Execution comes to a stop only after the event thread returns to
// the event loop.
//
// A coroutine object that is not executing and is not reachable is
// garbage collected.

// Create a new coroutine but do not start it.  fn must be a function,
// or an exception is thrown.  There is an implicit
// 'Coroutine_yieldTo(null, null)' after fn returns.

function Coroutine_make(fn) { ... }

// Kill the coroutine c.  If c is the currently executing routine, or
// if c is null or is not a coroutine, an exn is thrown on the event
// thread.  (It's OK if c is a dead coroutine.)

function Coroutine_destroy(c) { ... }

// Return the currently executing coroutine.  Returns null if the
// current coroutine is the event thread.

function Coroutine_self() { ... }

// Transfer control to a coroutine and suspend the current coroutine.
// If c had not been started then invoke its 'fn' on v.  Otherwise, c
// has already yielded with Coroutine$yieldTo; return v from that
// call.  The value 'null' for c denotes the event thread.  If c is
// an exited coroutine or not a coroutine then an exception is thrown
// on the event thread.

function Coroutine_yieldTo(c, v) { ... }

// Transfer control to a coroutine and destroy the current coroutine.
// The current coroutine cannot be the event thread.  The current
// coroutine must not be yielded to again.  If c had not been started
// then invoke its 'fn' on v.  Otherwise, c has already yielded with
// Coroutine$yieldTo; return v from that call.  The value 'null' for c
// denotes the event thread.  If c is an exited coroutine or not a
// coroutine then an exception is thrown on the event thread.

function Coroutine_exitTo(c, v) { ... }

