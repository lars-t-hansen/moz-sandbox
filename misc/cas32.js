var fa = new SharedFloat32Array(10);

function f(a, b) {
    // compareExchange is always used for its value
    return Atomics.compareExchange(fa, 2, a, b);
}

var sum = 0;
for ( var i=0 ; i < 10000 ; i++ ) {
    sum += f(0, 1.5);
    sum += f(1.5, 0);
}

assertEq(sum, 15000);

function g(k) {
    return Atomics.load(fa, k);
}

var sum = 0;
fa[0] = 1.5;
fa[1] = 2.75;
for ( var i=0 ; i < 10000 ; i++ ) {
    sum += g(0);
    sum += g(1);
}

assertEq(sum, 15000 + 27500);

function h(k, v) {
    Atomics.store(fa, k, v);
    return Atomics.load(fa, k);
}

var sum = 0;
for ( var i=1 ; i <= 10000 ; i++ ) {
    sum += h(0, i);
    sum += h(1, i*0.5);
}

assertEq(sum, (10000*10001)/2 + (10000*10001)/4);
