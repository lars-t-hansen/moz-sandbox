var T = TypedObject;
var Q = new T.StructType({x:T.float64, y:T.float64, z:T.float64, w:T.float64});

function testit(i) {
    return new Q({x:i*1.5, y:0.2, z:3.3, w:4.8});
}

function test() {
    var sum=0;
    for ( var i=0 ; i < 18500000 ; i++ )
	sum += testit(i).x;
    return sum;
}

print(test());
