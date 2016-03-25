// Must test asm.js also.
// Must test variable indices also.

/*
function f(ia, k) {
    Atomics.add(ia, 10, 1);
    Atomics.add(ia, k, 1);
}
*/

var sum;
function g(ia, k) {
    /*
    sum += Atomics.and(ia, 0, 1337);
    sum += Atomics.and(ia, k, 1337);
    */
    sum += Atomics.add(ia, 0, 1337);
    sum += Atomics.add(ia, 0, k);
    //sum += Atomics.and(ia, k, 1337);
}

var ia = new SharedInt8Array(11);
for ( var i=0 ; i < 10000 ; i++ ) {
    //f(ia, i % 10);
    g(ia, i % 10);
}

print("Done");

