Work-in-progress on a baseline compiler for Wasm.

The patch set in patches.bundle is kept up-to-date with
mozilla-inbound every few days.  Apply this to mozilla-inbound by
pulling from it.

At the moment the baseline compiler is just a drop-in replacement for
js/src/asmjs/WasmIonCompile.cpp in Firefox.  With these patches
applied, the baseline compiler is used instead of the Ion compiler.

NOTE: At the moment there is no profiling and no switch-over to
Ion-compiled code.

For a full accounting of the status, see the comment at the beginning
of js/src/asmjs/WasmBaselineCompile.cpp.
