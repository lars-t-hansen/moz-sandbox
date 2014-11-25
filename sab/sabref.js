function t(arr, i) {
    arr[i+2] = arr[i] + arr[i+1];
}

function fib(n) {
    var k0 = 0;
    var k1 = 1;
    for ( var i=0 ; i < n ; i++ ) {
	var t = k0+k1;
	k0 = k1;
	k1 = t;
    }
    return k0;
}

var arr = new SharedInt32Array(1024);
arr[1] = 1;
for ( var i=0 ; i < 1000 ; i++ )
    t(arr, i);
for ( var i=0 ; i < 40 ; i++ ) // Soon after 40 there's an overflow in the int32 array
    assertEq(arr[i], fib(i));
