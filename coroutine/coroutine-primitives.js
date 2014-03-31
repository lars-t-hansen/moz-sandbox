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
 * there that might work, or might serve as templates.
 *
 * The requirements on the runtime are:
 *
 * - the runtime must be aware of multiple stacks (GC roots, debugging
 *   contexts)
 *
 *  - the runtime must be reentrant in this sense: if we can call
 *    JS-to-C++-to-JS then the C++ bits must be completely reentrant,
 *    the JS callout must not be restricted in what it does.
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

/**
 * Create a coroutine, suspend the current one, switch to the new
 * one, and invoke the 'thunk' on it.  If 'deep' is true then a
 * large stack is created.
 *
 * Unchecked requirements:
 *   - thunk must be a callable
 *   - thunk must never return
 */
function CoroutinePrimitive_start(deep, thunk) { ... }

/**
 * Destroy a coroutine.
 *
 * Unchecked requirements:
 *   - co must have been created by CoroutinePrimitive_start
 *   - co must not have been destroyed subsequent to creation
 *   - co != CoroutinePrimitive_self()
 */
function CoroutinePrimitive_destroy(co) { ... }

/**
 * Get the current coroutine, or null if the current coroutine is
 * the event thread, which was never "created by CoroutinePrimitive_start".
 */
function CoroutinePrimitive_self() { ... }

/**
 * Suspend the current routine, switch to co, and return w from co's
 * call to CoroutinePrimitive_resume.  When some coroutine resumes the
 * current routine with a value, capture the value as v.  co may be
 * null.
 *
 * Unchecked requirements:
 *   - co must have been created by CoroutinePrimitive_start
 *   - co must not have been destroyed subsequent to creation
 *   - co != CoroutinePrimitive_self()
 */
function CoroutinePrimitive_resume(co, w) { ... }
