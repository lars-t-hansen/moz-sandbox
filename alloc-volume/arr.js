function testit(i) {
    return [i*1.5, 0.2, 3.3, 4.8];
}

function test() {
    var sum=0;
    for ( var i=0 ; i < 18500000 ; i++ )
	sum += testit(i)[0];
    return sum;
}

print(test());
