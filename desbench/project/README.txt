This was a class project for a crypto class.  It partially evaluates
the DES algorithm with respect to a key and generates a C program
which is then a fast encryptor for that key.

There are several variants of the partial evaluator, the last versions
are descrypt8 and descrypt9.  The latter generates (significantly)
larger data tables but also a faster encryptor.  YMMV.

You need a reasonable scheme system.  This was orginally (1996)
developed for Chez Scheme, it has most recently (2017) been tested for
Larceny (larcenists.org).

What you do is you load a set of Scheme files and run a function to
generate C code.  You then compile the C code.  See eg descrypt9m.sch
for further instructions, and Makefile.

For a WebAssembly target, use WasmMakefile instead.

An example of the output is in descrypt9m.c.
