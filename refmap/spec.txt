@lukewagner and I were tossing around some ideas for weakly held
objects and finalization.  Currently the main wasm use case is for
wrapping C++/Rust/whatever libraries compiled to wasm and providing a
JS interface to them.  In this case, there will be an address in the
wasm heap representing an important program object, and a JS facade
object (trying to avoid the overloaded term "proxy" here) that knows
that address.  The problem is then twofold: when the JS facade is GC'd
we would want to run some wasm code to destruct the wasm
representation; and when wasm calls out to JS passing it a pointer (or
for that matter returns a pointer to a JS caller) we want to map that
pointer to the existing JS object, so that there can be a single
facade per important wasm program object.

We're not sure we want to open up the pandora's box that is weak refs
and finalization in JS, so here's a modest proposal to provide
something germane to host-bindings:

We have a new type, `WebAssembly.ReferenceMap` (nobody is wedded to
this name).  There can be many instances of this type.  The reference
map maps wasm values (ie, i32/i64/f32/f64) to JS objects.  In the
following let `r` be an instance of ReferenceMap.

We add a JS object to the map with `r.add(wasmValue, jsObject)`.  If
the wasmValue is in the map already there's an error or it's replaced
(discuss).  In principle `jsObject` could be almost any JS value that
is subject to GC, not just Object values, but "subject to GC" is a
little slippery (discuss).

It's likely there also needs to be a `r.remove(wasmValue)` method.

We retrieve the JS value associated with some wasm value through
`r.get(wasmValue)`, which returns undefined or the value.  So this
method doubles as a membership query method.

The JS objects are held weakly by the ReferenceMap.  When such an
object is no longer visible to JS [*], the wasm value that maps to it
is moved into the ReferenceMap's "dead set".  The dead set can be
queried from JS to yield an array of wasm values: `r.reap()` returns
an Array of wasm values that were the keys of objects that have been
collected.  (This API can be made more efficient, so don't get hung up
on that.)  Periodically the application will do housekeeping and check
whether there are objects to reap; if so it will perform some action
to deallocate the wasm storage for each of the values in the reaped
set.

End proposal.

Notes:

Arguably the proposal is a variant of Guardians [1] but with the wasm
"object" preserved on GC, not the JS object that was GC'd.  We avoid
all issues of whether objects should be "resurrectable" or what the
object state may be after it has been deemed collectible.  Of course,
if there can be multiple JS facades referencing a wasm object then the
wasm object must be refcounted or similar; there must exist some
protocol for how to destruct it safely.  This may also be the case if
the facades are in different agents and the wasm memory is shared.

We assume no restrictions on whether objects can be registered with
multiple ReferenceMaps under same or different keys.

[*] The timing of reclaiming dead objects and moving values to the
dead set is tricky.  We can do this between turns.  We can also
stipulate that it may happen when `reap()` is called (this allows
programs to not return to the event loop, often desirable in workers).
But it can't happen automatically when wasm code is on the stack, for
example, since code that calls in to wasm while an object is in the
set will expect that if wasm calls back out to JS the object should
still be in the set.  That is, liveness of that value is implicit.
While there's going to be some code that extracts the raw pointer from
the JS object to pass it to wasm, the variable holding that raw
pointer may go out of scope (or be considered dead) after the pointer
was extracted.  JS makes much the same choice - GC changes only
observable between turns. [check this]

If the object is weakly held through eg weakmaps then the criterion
for being removed from the ReferenceMap is that it is no longer
reachable via other weak pointers.  Thus if a WeakMap can only be
updated between turns, an object that is also held in a WeakMap cannot
be moved to the dead set by means of `reap()`.

[1] https://www.cs.indiana.edu/~dyb/pubs/guardians-pldi93.pdf
