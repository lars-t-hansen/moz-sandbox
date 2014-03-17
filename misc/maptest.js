// Compares the performance of typed and untyped map on simple types.
// Developed for https://bugzilla.mozilla.org/show_bug.cgi?id=983577

if (!this.TypedObject)
    quit();

var k = 1000;
var A = new Array(k);
var AT = new TypedObject.ArrayType(TypedObject.int32);
var B = new AT(k);
for ( var i=0 ; i < k ; i++ ) {
    A[i] = i;
    B[i] = i;
}

function mapper(iterations, array) {
    var dummy = 0;
    for ( var i=0 ; i < iterations ; i++ ) {
	var r = array.map(x => x+1);
	dummy += array[array.length-1];
    }
    return dummy;
}

var itit=10;
var it=1000;
var typed = [];
var untyped = [];
for ( var i=0 ; i < itit ; i++ ) {
    var t0 = Date.now();
    mapper(it, A);
    var t1 = Date.now();
    mapper(it, B);
    var t2 = Date.now();
    untyped.push(t1-t0);
    typed.push(t2-t1);
}

print("Untyped: " + untyped.join(" "))
print("Typed: " + typed.join(" " ));
print("Untyped/typed: " + untyped.reduce((x,y) => x+y) / (typed.reduce((x,y) => x+y)));
