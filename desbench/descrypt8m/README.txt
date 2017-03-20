For the C program:

~/moz/mozilla-inbound/js/src/build-release/dist/bin/js -f larceny.js -f vectors.js -f basis.js -f meta.js -f number.js -f descrypt8m.js -f suffix.js > descrypt8m.c

make t8  # testing
./t8

make r8  # benchmarking
./r8


For the Wast program:

~/moz/mozilla-inbound/js/src/build-release/dist/bin/js -f larceny.js -f vectors.js -f basis.js -f meta-wast.js -f number.js -f descrypt8m.js -f suffix-wast.js
