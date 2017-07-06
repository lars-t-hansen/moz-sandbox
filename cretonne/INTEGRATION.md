Notes from a meeting with Jakob
-------------------------------

We'll use the js shell as a vehicle for integration, for now.  It saves effort.

People will need to checkout Cretonne source in js/src/cretonne
(details TBD) and build it separately (ditto).

People will need to apply a patch to js/src to add code and
definitions to SpiderMonkey (details TBD).

We will add a switch --wasm-cretonne to the JS shell that compiles
wasm with Cretonne only (no baseline, no ion)

There will be js::wasm::CretonneCompileFunction() that encapsulates
Cretonne and works in the same way as IonCompileFunction and
BaselineCompileFunction.

It's nice if some of this can be landable behind suitable defines
(NIGHTLY_BUILD; WASM_CRETONNE) but not essential.

Dennis & Jakob work on what's inside CretonneCompileFunction, probably
a C++ shim around Rust code.

We'll bolt on a call to the validation pass before
CretonneCompileFunction(), so that we get proper validation.

SpiderMonkey / Baldr owns data representations (notably,
ModuleEnvironment).  The wasm->cretonne code will have to cope.
Cretonne needs access to fields of signatures, tabledescs,
globaldescs, at least, but usually very simple stuff.  Jakob suggests
that Denis defines a trait of the things he needs and that Lars
implements these.

