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
