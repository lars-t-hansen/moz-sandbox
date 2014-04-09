var T = TypedObject;
var ST = new T.StructType({x: T.int32, y: T.int32});
var AT = new T.ArrayType(ST);
function f() {
    var A = new AT(1000);
    var O = new ST({x:5, y:7});
    var B = A.mapPar(function (elt,i,a,cursor) { return O; });
    print(O.x);
    print(O.y);
    for ( var i=0 ; i < B.length ; i++ )
	if (B[i].x != 5 || B[i].y != 7)
	    throw new Error("Element " + i + ": x=" + B[i].x + ", y=" + B[i].y);
}

for ( var i=0 ; i < 100 ; i++ ) {
    print("ITERATION " + i);
    f();
    print("");
}
