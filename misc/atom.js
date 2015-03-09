// Must test asm also.

function f(ia) {
    Atomics.add(ia, 0, 1337);
}

var sum;
function g(ia) {
    sum += Atomics.and(ia, 0, 1337);
}

var ia = new SharedInt8Array(1);
for ( var i=0 ; i < 10000 ; i++ ) {
    f(ia);
    g(ia);
}

