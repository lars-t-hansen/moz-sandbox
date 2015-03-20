var fa = new SharedFloat64Array(10);

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
