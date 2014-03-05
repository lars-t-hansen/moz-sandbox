// See https://bugzilla.mozilla.org/show_bug.cgi?id=966567
//
// Based on GC logs this allocates about 1.7GB in the JS shell, the same as the array case.
// The very best case is probably around 740MB (four doubles and a header).

function testit(i) {
    return new {x:i*1.5, y:0.2, z:3.3, w:4.8};
}

function test() {
    var sum=0;
    for ( var i=0 ; i < 18500000 ; i++ )
	sum += testit(i).x;
    return sum;
}

print(test());
