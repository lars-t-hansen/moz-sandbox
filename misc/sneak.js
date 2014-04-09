/* Exposes left-over results from bailouts */

var T = TypedObject;
var ST = new T.StructType({x: T.int32, y: T.int32});
var AT = new T.ArrayType(ST);
function f() {
    var A = new AT(1000);
    var x = { blah: 10 };
    var B = A.mapPar(function (elt,i,a,cursor) { if (i == 373) x.blah = 12; cursor.x = i; cursor.y += i; });
    for ( var i=0 ; i < B.length ; i++ )
	if (B[i].x != B[i].y)
	    print(i + " " + B[i].x + " " + B[i].y);
}

for ( var i=0 ; i < 10 ; i++ ) {
    print("ITERATION " + i);
    f();
    print("");
}

