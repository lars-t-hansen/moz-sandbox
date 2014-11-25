function a(ta) {
    var x = ta[0];
    Atomics.fence();
    var y = ta[1];
    var z = y + 1;
    var w = x + z;
    return w;
}

var sab = new SharedArrayBuffer(4096);
var ia = new Int32Array(sab);
var ib = new Int32Array(1);
var xs = [];
for ( var j=0 ; j < ia.length ; j++ )
    ia[j] = 37;
ib[0] = 42;
for ( var i=0 ; i < 1000 ; i++ )
    xs.push(ia);
xs.push(ib);
var v = 0;
for ( var i=0 ; i < 1001 ; i++ )
    v += a(xs[i]);
print(v);
print(1000*(37*2+1) + 42);
