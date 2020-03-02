# Linear-memory tracing GC support

How about this:

* There is a new type 'p32' which is the same size as 'i32'
* Locals can be of this type, its initial value is 0, which is the null pointer
* There are transfer instructions i32.from_p32 and p32.from_i32 so as to allow
  pointer arithmetic on the int representation
* There are load and store instructions p32.init, p32.store, p32.clear, and
  p32.load that store to and load from linear memory.  'init' is for
  initializing stores (the old cell value is known to be null or non-pointer
  garbage) and 'clear' stores a known null (may be redundant but may also be
  useful, TBD).

The load and store instructions imply the necessary barriers for the host GC.
It is possible to use i32.load and i32.store on the locations, but DOING SO WILL
INVALIDATE THE GC STATE.

Since the types are distinct, pointer values in registers can be known to the
compiler and anywhere a safepoint is needed in the machine code, the pointer
values can be stored in the stack so that they can be found by the GC.

(Obviously this is all very similar to the reftypes in that regard.)

There is a mapping from a pointer value to its object value.  This mapping must
be part of the object's type.  For example, we could consider these distinct
types:

* p32   - all bits are significant, pointer points to object head
* p32/3 - low three bits are ignored (tag)
* p32/i - interior pointer

