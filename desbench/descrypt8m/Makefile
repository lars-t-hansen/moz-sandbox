# To build the C program for benchmarking, make 'r8'
#
# To build the C program for testing, make 't8'
#
# To generate and run the wasm program in a shell, make 'descrypt-wast'.  See further information
#   in suffix-wast.js for how to configure that for testing or benchmarking.

JS=~/moz/mozilla-inbound/js/src/build-release/dist/bin/js
JSFLAGS=
CFLAGS=-O3 -g

r8:	main.o descrypt8m.o
	cc -o r8 main.o descrypt8m.o

t8:	testmain.o descrypt8m.o
	cc -o t8 testmain.o descrypt8m.o

	.dummy: descrypt-wast
descrypt-wast:
	$(JS) $(JSFLAGS) -f prefix.js -f vectors.js -f basis.js -f meta-wast.js -f number.js -f descrypt8m.js -f suffix-wast.js

clean:
	rm -f *~ *.o r8 t8 descrypt8m.c

main.o:	main.c Makefile
	cc $(CFLAGS) -c main.c

testmain.o:	testmain.c Makefile
	cc $(CFLAGS) -c testmain.c

descrypt8m.o:	descrypt8m.c Makefile
	cc $(CFLAGS) -c descrypt8m.c

descrypt8m.c:	prefix.js vectors.js basis.js meta.js number.js descrypt8m.js suffix.js Makefile
	$(JS) $(JSFLAGS) -f prefix.js -f vectors.js -f basis.js -f meta.js -f number.js -f descrypt8m.js -f suffix.js > descrypt8m.c

