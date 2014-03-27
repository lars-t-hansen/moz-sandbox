/**
 * Primitive coroutine interface, provided by the system.
 * 2014-03-26 / lhansen@mozilla.com
 */

/**
 * API level.
 *
 * A Coroutine represents an explicitly scheduled thread of control
 * with a program state and a stack.  Only one coroutine is ever
 * active at the same time within a runtime.  Coroutines are tied
 * to a runtime.
 *
 * A coroutine has one of four states: CREATED, ACTIVE, SUSPENDED, and
 * DESTROYED.  A routine created by Coroutine_make() starts in the
 * CREATED state.  Once it has been resumed, either for the first time
 * or subsequently, it becomes ACTIVE.  It becomes SUSPENDED when it
 * yields to another coroutine.  It becomes DESTROYED once it is
 * destroyed by Coroutine_destroy() or yields to another coroutine
 * with Coroutine_exitTo().
 *
 * The coroutine representing the runtime's event thread is represented
 * by the value null; all other coroutines are represented by a
 * Coroutine object.
 *
 * Execution in a runtime comes to a stop only after the event thread
 * returns to the event loop; coroutines that are not garbage continue
 * to live past that point.
 *
 * A coroutine object that is not ACTIVE and is not reachable is
 * garbage and will be collected, at which point its resources will be
 * reclaimed.
 *
 * If the currently executing coroutine calls a blocking runtime
 * function then it will block as per normal: no preemption or
 * callback occurs to "suspend" the coroutine automatically across the
 * blocking call so that other code can run on the coroutine thread.
 * Coroutines are fully cooperative-only.
 */

/**
 * Primitive layer.
 *
 * There is a lower, unsafe layer, exposing four primitives:
 *
 * v = CoroutinePrimitive_start(deep, thunk)
 *
 *    Create a coroutine, suspend the current one, switch to the new
 *    one, and invoke the 'thunk' on it.  If 'deep' is true then a
 *    large stack is created.
 *
 *    Unchecked requirements:
 *      - thunk must be a callable
 *      - thunk must never return
 *
 * CoroutinePrimitive_destroy(co)
 *
 *    Destroy a coroutine.
 *
 *    Unchecked requirements:
 *      - co must have been created by CoroutinePrimitive_start
 *      - co must not have been destroyed subsequent to creation
 *      - co != CoroutinePrimitive_self()
 *
 * co = CoroutinePrimitive_self()
 *
 *    Get the current coroutine, or null if the current coroutine is
 *    the event thread, which was never "created by
 *    CoroutinePrimitive_start".
 *
 * v = CoroutinePrimitive_resume(co, w)
 *
 *    Suspend the current routine, switch to co, and return w from
 *    co's call to CoroutinePrimitive_resume.  When some coroutine resumes
 *    the current routine with a value, capture the value as v.  co
 *    may be null.
 *
 *    Unchecked requirements:
 *      - co must have been created by CoroutinePrimitive_start
 *      - co must not have been destroyed subsequent to creation
 */

/**
 * Implementation notes.
 *
 * On Unix-like systems, the primitives can be implemented on top of
 * makecontext and swapcontext; on Windows they can be implemented on
 * top of fibers, probably.  There are other abstraction libraries out
 * there that might work, or might serve as templates.
 *
 * The requirements on the runtime are:
 *  - the runtime must be aware of multiple stacks (GC roots, debugging contexts) 
 *  - the runtime must be reentrant in this sense: if we can call
 *    JS-to-C++-to-JS then the C++ bits must be completely reentrant,
 *    the JS callout must not be restricted in what it does, including
 *    killing the stack; effectively it must be legal to reenter the
 *    C++ code while an activation is already on the stack, and it
 *    must be legal to longjmp across that code later.  This is
 *    probably true already.
 */

/**
 * Create and return a new coroutine in the CREATED state.  fn must be
 * a callable, or an Error is thrown.  When the coroutine is resumed
 * the first time fn will be invoked.  If fn returns V the coroutine
 * will call Coroutine_exitTo(null, V).  If fn throws a value E the
 * coroutine will call Coroutine_exitTo(null, E).
 * 
 * The flags are optional.  Each flag is a string requesting
 * non-default behavior.  These are the flags:
 *
 *   "deep" - Request a deep stack for the coroutine.  The default
 *            is fairly moderate, perhaps 128KB.
 *
 * @see Coroutine_yieldTo and Coroutine_exitTo().
 */
function Coroutine_make(fn, ...flags) { 
    if (!IsCallable(fn))
	throw new Error("Not callable");
    var deep = false;
    for ( var f of flags.length )
	if (f === "deep")
	    deep = true;
    var caller = Coroutine_self();
    var c = new Coroutine();
    var co =
	CoroutinePrimitive_start(
	    deep,
	    function () {
		var self = Coroutine_self();
		try {
		    var v = Coroutine_yieldTo(caller, self);
		    var r = fn(v);
		    Coroutine_exitTo(null, r);
		}
		catch (e) {
		    Coroutine_exitTo(null, e);
		}
	    });
    c.state_ = "created";
    c.co_ = co;
    return c;
}

/**
 * Destroy the coroutine c.  If c is the currently executing
 * coroutine, or if it is null or is not a Coroutine object, then an
 * Error is thrown.  (It's OK if c is a Coroutine in the destroyed
 * state.)
 */
function Coroutine_destroy(c) {
    if (c === null || !(c instanceof Coroutine) || c.state_ === "active")
	throw new Error("Not a destroyable coroutine");
    c.state_ = "destroyed";
    CoroutinePrimitive_destroy(c.co_);
    c.co_ = null;
}

/**
 * Return the state of the coroutine in the form of a string,
 * either "created", "active", "suspended", or "destroyed".
 * If c is not null or a Coroutine object then an Error is thrown.
 */
function Coroutine_state(c) {
    if (!(c === null || c instanceof Coroutine))
	throw new Error("Not a coroutine");
    if (c === null)
	return Coroutine_self() === null ? "active" : "suspended";
    return c.state_;
}

/**
 * Return the currently executing coroutine.  Returns null if the
 * current coroutine is the event thread.
 */
function Coroutine_self() {
    return CoroutinePrimitive_self();
}

/**
 * Transfer control to ("resume") a coroutine and suspend the current
 * coroutine.  If c is in the CREATED state then invoke its callable
 * on v.  Otherwise, c has already yielded with Coroutine_yieldTo();
 * return v from that call.  If c is a destroyed coroutine or not a
 * coroutine then an Error is thrown.
 */
function Coroutine_yieldTo(c, v) {
    if (!(c === null || (c instanceof Coroutine && c.state_ !== "destroyed")))
	throw new Error("Not a resumable coroutine");
    var self = Coroutine_self();
    if (self)
	self.state_ = "suspended";
    if (c)
	c.state_ = "active";
    var { toDestroy, value } = CoroutinePrimitive_resume(c === null ? null : c.co_, { toDestroy: false, value: v });
    if (c)
	c.state_ = "suspended";
    if (self)
	self.state_ = "active";
    if (toDestroy)
	Coroutine_destroy(toDestroy);
    return value;
}

/**
 * Transfer control to ("resume") a coroutine and destroy the current
 * coroutine.  If c is in the CREATED state then invoke its callable
 * on v.  Otherwise, c has already yielded with Coroutine_yieldTo();
 * return v from that call.  If c is a destroyed coroutine or not a
 * coroutine, or if the current coroutine is the event thread, then an
 * Error is thrown.
 *
 * Invariant: this function never returns to the caller, no matter what.
 */
function Coroutine_exitTo(c, v) { 
    var self = Coroutine_self();
    if (self === null)
	throw new Error("Not a destroyable coroutine");
    if (!(c === null || (c instanceof Coroutine && c.state_ !== "destroyed")))
	throw new Error("Not a resumable coroutine");
    CoroutinePrimitive_resume(c === null ? null : c.co_, { toDestroy: self, value: v });
    throw new Error("Coroutine that should have been destroyed was resumed");
}
