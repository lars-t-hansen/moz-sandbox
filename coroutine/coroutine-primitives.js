/**
 * Primitive layer.
 *
 * This is a lower, unsafe layer, exposing four primitives and a
 * primitive coroutine representation (a reference type).
 *
 * When a primitive coroutine object is garbage collected the
 * coroutine is automatically destroyed.
 */

/**
 * Implementation notes.
 *
 * On Unix-like systems, the primitives can be implemented on top of
 * makecontext and swapcontext; on Windows they can be implemented on
 * top of fibers, probably.  There are other abstraction libraries out
 * there that might work, or might serve as templates.  Clearly
 * coroutines can be implemented (expensively) on top of threads.
 *
 * The requirements on the runtime are:
 *
 * - the runtime must be aware of multiple stacks (GC roots, debugging
 *   contexts)
 *
 *  - the runtime must be reentrant in this sense: if we can call
 *    JS-to-C++-to-JS then (a) the C++ bits must be completely
 *    reentrant, the JS callout must not be restricted in what it
 *    does, and (b) there must be no assumption about one of these JS
 *    or C++ activation being nested within another.
 *
 * Jason says that we cannot just kill the stack when a primitive
 * coroutine is destroyed: there will be C++ destructors on the
 * coroutine's stack that must be unwound.  There is a (bad) protocol
 * for this that the script timeout logic uses; basically it sets some
 * exception context(?) flag to false and throws, and JS exception
 * handlers just ignore the exception and do not execute finally
 * clauses, while C++ code returns through its logic as it should.
 *
 * Jason also says that stacks can get very deep especially when the
 * debugger is used.  This is a concern obviously; we'll need to set
 * aside fairly large stacks for coroutines, probably the same size as
 * for workers.  Decommitting will be helpful.
 */

/* A primitive coroutine represents a thread of control in an address
 * space.  It shares the heap memory and global variables / global
 * state with other coroutines but has its own dynamic state (stack,
 * registers).
 *
 * Only one coroutine is active at a time within the address space.
 *
 * Switching from one coroutine to another is entirely under program
 * control, there is no preemption.
 *
 * The "null coroutine" represents the main thread of control in the
 * address space, the brower's event thread.  It is represented by the
 * null value.
 *
 * All other coroutines are represented by unique non-null values (not
 * necessarily object values).
 *
 * A coroutine is in one of three states, ACTIVE, SUSPENDED, or
 * TERMINATED:
 * 
 * - The currently active coroutine is always ACTIVE
 * - A coroutine whose initial thunk has not returned and which has
 *   not been destroyed is SUSPENDED
 * - All other coroutines are TERMINATED
 *
 * The null coroutine cannot enter the TERMINATED state.
 */

/**
 * Create a coroutine, suspend the current one, resume (start) the new
 * one, and invoke the "thunk" on it.
 *
 * If coroutine co's "thunk" returns normally or throws then an Error
 * is thrown on the null coroutine and co enters the TERMINATED state.
 *
 * Unchecked requirements:
 *   - thunk must be a callable
 */
function CoroutinePrimitive_start(thunk) { ... }

/**
 * Destroy a coroutine, moving it into the TERMINATED state.
 *
 * Unchecked requirements:
 *   - co must be a possible non-null return value from CoroutinePrimitive_self()
 *   - co's state must be SUSPENDED
 */
function CoroutinePrimitive_destroy(co) { ... }

/**
 * Suspend the current routine, switch to co, and return w from co's
 * call to CoroutinePrimitive_resume().  When some coroutine resumes the
 * current routine with a value, capture the value as v.  co may be
 * null.
 *
 * Unchecked requirements:
 *   - co must be a possible return value from CoroutinePrimitive_self()
 *   - co's state must be SUSPENDED
 */
function CoroutinePrimitive_resume(co, w) { ... }

/**
 * Get the value representing the current coroutine, this will be null
 * if the current coroutine is the event thread and otherwise a unique
 * (over the course of the computation) non-null value.
 */
function CoroutinePrimitive_self() { ... }
