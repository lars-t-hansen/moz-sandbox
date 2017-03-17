This was a class project for a crypto class.  It partially evaluates
the DES algorithm with respect to a key and generates a C program
which is then a fast encryptor for that key.

There are several variants of the partial evaluator, the last versions
are descrypt8m and descrypt9m.  The latter generates (significantly)
larger data tables but also a faster (by 15%) encryptor.  YMMV.

You need a reasonable scheme system.  This was orginally (1996)
developed for Chez Scheme, it has most recently (2017) been tested for
Larceny (larcenists.org).

What you do is you load a set of Scheme files and run a function to
generate C code.  You then compile the C code.  See eg descrypt9m.sch
for further instructions, and Makefile.  You can also run the DES
algorithms in Scheme by loading eg descrypt9.sch instead but this is
mostly useful for testing.

For a WebAssembly target, compile the C code with WasmMakefile
instead.

Examples of the output are in descrypt8m.c and descrypt9m.c.

Everything was originally configurable for 32-bit or 64-bit, but
various 64-bit assumptions have probably crept in here and there by
now.  Buyer beware.
