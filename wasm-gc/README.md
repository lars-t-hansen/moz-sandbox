-*- fill-column: 80 -*-

# Linear-memory tracing GC support

How about this:

* There is a new type 'p32' which is the same size as 'i32'.  This is the offset
  in linear memory of some "object" that the GC can scan
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
* p32/1 - low bit is ignored, ditto
* p32/2 - low two bits are ignored, ditto
* p32/3 - low three bits are ignored, ditto
* p32/i - all bits are significant, pointer points somewhere in object (but not
          past it)

It'd be easy for most implementations to support p32 and *maybe* p32/N (some
implementations actually use tag bits for their own purposes), but p32/i might
be harder for many.  There are various glosses on this:

* p32/i/K - interior pointer to object of size K, allows for size-segregation,
            but K is tricky if the GC gets to control the actual size of an
	    object based on its structure


For p32 and p32/N, we should allow a constant offset parameter that can be used
to compensate for a known tag in load and store instructions.

One question is whether the p32.load/p32.store/etc instructions can be used
outside of "objects" and thus be used to create and remove GC roots (that's part
of the motivation for the "init" and esp the "clear" instruction) or whether
root management should be separate, probably by separate instructions.

Another question is what an "object" is.  I like Wouter's idea that objects are
segregated onto separate pages and that the GC gets to lay out objects as it
wants, and gets to add header fields and metadata.  At the same time, I'd like
to support headerless objects and BiBoP allocation.