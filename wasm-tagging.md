* Tagged pointers in WebAssembly

** Background: segregated heap of typed objects

We assume an opaque gc'd heap of TypedObject-like objects.  References
into this heap are exposed to Wasm in the form of a new "Ptr" type.

(An object field is read or written via a Ptr with typed operations
that operate on a Ptr and some type of object offset.  Thus there
needs to be some way to associate static type information with a Ptr,
with a location that stores a Ptr, or with an operation on a Ptr,
and/or there would also be a way of asserting -- with a run-time check
-- that a Ptr is of a particular type, for downcasting.  We will
ignore these concerns here.)

A Ptr can also be null.


** Use cases for tagged pointers; prior art

The use cases are mostly dynamically typed languages like Lisp and
Prolog, but sometimes also statically typed systems that deeply need
to optimize space consumptions (pointer-plus-flag representations).

There are intermediate cases; statically typed languages sometimes use
dynamic-ish representations for implementation simplicity, using a
single tag bit to distinguish pointers and non-pointers, thereby
allowing registers and stack slots to contain either type of value
without having to worry about stack maps for garbage collection.

I'll concentrate on dynamic language use cases, because the other use
cases are easily served.  There are many surveys of tagging schemes in
the literature; Google is your friend.

Lisp and Prolog will typically want to store the following value
classes in a tagged word:

- pointer to gc'd storage plus major type class (cons, pointer-containing vector, pointer-free vector)
- small integer
- other small immediate (booleans, characters, special values)

Some systems may wish to store larger values in the word, eg,
floating-point numbers, to avoid heap allocation.

A variety of tagging systems exist:

- In low-tagging systems, the low bits of a pointer to aligned storage
  are zero and can hold a tag.  Tags are comparatively small, 3-4
  bits.

- In high-tagging systems, we assume (via VM allocation tricks or
  architecture invariants) that the upper pointer bits are all zero,
  the high bits can store a tag.  When this works, tags tend to be
  comparatively large, easily 8 bits.  (NaN-boxing is a particular
  instance of this.)

- In BiBoP-tagging systems, the high bits of a pointer are part of the
  pointer but also indicate the type of the pointed-to datum; objects
  of different types are allocated in different parts of the virtual
  address space.

Combinations of low-tagging with the other two types is usually possible.

Low-tagging will push the minor type class into the object and will
therefore tend to encourage objects with larger headers or with
meta-fields; high-tagging and BiBoP-tagging avoid headers to a larger
extent, or allow them to be smaller.  A common use of BiBoP-tagging is
to support headerless heap-allocated floating point numbers.

Low-tagging is generally portable; the others less so.  High-tagging
is extremely attractive if the architecture masks off the high bits of
a pointer, but becomes a mess when the next revision of the
architecture stops doing so; it also looks attractive when the OS does
not allocate application data to high-memory virtual addresses, but
starts failing when the OS changes its mind about that.  (SpiderMonkey
is having problems with its NaN-boxing on SPARC and aarch64 and will
have additional problems when the x64 architecture expands its address
space to 52 bits.)

The "operations" on tags are tag extraction, tag masking (removal),
and tag insertion.  Tag assignments vary widely and are driven by
application needs.  A "general" system is hard to design, but
low-tagging has particularly simple instruction sequences for the tag
operations on current hardware.


** Security and integrity

If a tagged pointer can represent the union of a pointer with other
data (eg integer) then we must ensure that there is no way to extract
the pointer bits as an integer, nor any way to make an integer
masquerade as a pointer.  This requires pointers to be distinguished
from other data in a privileged way.  Thus at least one tag value, but
possiblye one entire tag bit (the "pointer bit"), becomes reserved by
the system.


** Box, abstractly

A "Box" is a value that is guaranteed to have the same size as a Ptr.
(I'm not wedded to the name so let's not bikeshed that yet.)  Ignoring
the size requirement, the Box represents this structure:

```C
 struct Box {
   bool is_ptr;
   unsigned tag:TAGBITS;
   union {
     intptr_t ival:WORDSIZE-(TAGBITS+1);
     void* ptr;
   }
 }
```

(TAGBITS would reasonably be 2 on 32-bit systems and 2 or 3 on 64-bit systems.)

There are some obvious operations:

```C
  Box boxptr(void* ptr_, int tag_) { return Box { .is_ptr=true; .tag=tag_; .ptr=ptr_; } }
  Box boxint(intptr_t ival_, int tag_) { return Box { .is_ptr=false; .tag=tag_; .ptr=ptr_; } }

  int ptrval(Box b) { if (b.is_ptr) return b.ptr; return NULLPTR; }
  intptr_t intval(Box b) { if (b.is_ptr) return 0; return b.ival; }

  bool isptr(Box b) { return b.is_ptr; }
  bool isint(Box b) { return !b.is_ptr; }
  int tag(Box b) { return b.tag; }
```


** Box, concretely

In a practical implementation, the low bits of a Ptr would always be
zero and would have space for the is_ptr bit (lowest bit) and the tag
(next TAGBITS bits), and the ival would be shifted left by TAGBITS+1.
Suppose is_ptr is 1 for pointers and 0 for integers.  We then have
this layout with TAGBITS=2:

```
  +---------------------------+----------+--------+
  |   Pointer (WORDSIZE-3)    | Tag (2)  | Ptr(1) |
  +---------------------------+----------+--------+
```

We then get some operations that are easier to use for fast code:

```C
  intptr_t intbits(Box b) { if (b.is_ptr) return 1; return (b.ival << (TAGBITS+1)) | (b.tag << 1); }
  int tagbits(Box b) { return (b.tag << 1) | int(b.is_ptr); }
  Box fromintbits(intptr_t i) { return Box { .is_ptr=false; .tag = i >> 1; .ival = i >> (TAGBITS+1); }
```

Observe that, given the representation above, `intbits()` would
perform no shifting, masking, or field extraction for the value, only
for the type test; `tagbits()` would only extract the low bits with a
single mask; and `fromintbits()` would merely mask off the low bit.

On the other hand, we see that `ptrval()` must mask off the low bits
to extract the pointer.


** Avoiding type checks

Observe that a Box is immutable and that testing a Box `b` for
pointerness is just `b & 1`.  It seems plausible that reasonable
implementations can remove redundant pointerness checks and that if a
check can't be avoided it is cheap: just test the bit and branch.

Consider a straightforward tagged ADD in a Lisp-like language where
small integers use a zero tag (ignoring overflow issues):

```
  if (tagbits(a) == 0 && tagbits(b) == 0) 
    result = fromintbits(intbits(a) + intbits(b));
  else
    result = generic_add(a, b)
```

Here, `tagbits(x) == 0` implies `x` is not a pointer.  Some kind of
range analysis may be required for the compiler to avoid the
pointer-bit check in the two `intbits()` calls and the masking of the
pointer bit in `fromintbits()`, but it's at least plausible to do so,
if the compiler knows to look for it.


** Arithmetic and bit operations (speculative)

Consider again the tagged ADD from above:

```
  if (tagbits(a) == 0 && tagbits(b) == 0) 
    result = fromintbits(intbits(a) + intbits(b));
  else
    result = generic_add(a, b)
```

In addition to the issues with redundant tag checking and masking we
also have two conditional branches, which is not great.  We can
restructure:

```
  var a_ = intbits(a);
  var b_ = intbits(b);
  if ((a_|b_) == 0)
    result = fromintbits(a_ + b_);
  else
    result = generic_add(a, b)
```

but this isn't any better; now we have three conditional branches
unless we posit fairly advanced analysis that discovers that `a_` and
`b_` are only used under guard of a test that performs the testing
`intbits()` would otherwise perform.

The question becomes, should we have primitive operations that
explicitly support this cliche?  Is such an operation valuable enough?
On the abstract level this would be `(result, flag) = tagged_add(a, b)`
where `flag` would be true if the operation succeeded and false
otherwise.  (On the Wasm level I'm not sure it would return two values
or would be some kind of new control-flow operation like "add or jump if
add fails".)  And for arithmetic, what about overflow checking?

Ditto for other operations on small integers, which are common in
Lisp-like languages and will incur a substantial fraction of tag
checking: add, subtract, multiply, and, or, xor, and shifts.
