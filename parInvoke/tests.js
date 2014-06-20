// Test cases against samples.js

testeq("build", Array_static_build(10, (x) => x*2), [0, 2, 4, 6, 8, 10, 12, 14, 16, 18]);
testeq("map", Array_map.call([0,1,2,3,4,5], (x)=>x/2), [0, 0.5, 1, 1.5, 2, 2.5]);
testeq("map sliced", Array_map_sliced.call(iota(37), (x)=>x/2), iota(37).map((x) => x/2));
testeq("TO map sliced", TO_map_sliced.call(TO_float64_iota(37), (x)=>x/2), TO_float64_iota(37).map((x) => x/2));
testeq("reduce", Array_reduce.call([1,2,3,4,5], (a,b)=>a+b), 15);
testeq("mapreduce", Array_mapreduce.call([1,2,3,4,5], (a)=>a*2, (a,b)=>a+b), 30);
testeq("scan", Array_scan.call([1,2,3,4,5], (a,b)=>a+b), [1, 3, 6, 10, 15]);
testeq("filter", Array_filter.call([1,2,3,4,5], (a) => a%2 == 0), [2, 4]);
testeq("zip same", Array_static_zip([1,2,3],["a","b","c"], (x,y)=>[x,y]), [[1, "a"], [2, "b"], [3, "c"]]);
testeq("zip different",
       Array_static_zip([1,2,3],["a","b","c","d"], (x,y)=>[x,y]),
       [[1, "a"], [2, "b"], [3, "c"], [undefined, "d"]]);
testeq("rotate left", Array_rotate_left.call([1,2,3,4,5], 3), [4,5,1,2,3]);
testeq("flip by 1", Array_flip.call([1,2,3,4,5,6,7,8],1), [2,1,4,3,6,5,8,7]);
testeq("flip by 2", Array_flip.call([1,2,3,4,5,6,7,8],2), [3,4,1,2,7,8,5,6]);
testeq("flip by 4", Array_flip.call([1,2,3,4,5,6,7,8],4), [5,6,7,8,1,2,3,4]);

//

// Run manually for now

function testconvolve() {
    var h = 4;
    var w = 5
    var T = TypedObject.uint32.array(h,w);
    var g = new T;
    for ( var y=0 ; y < Math.min(h,w) ; y++ )
	g[y][y] = 1;
    var g2 = convolve(g);
    pgrid(g2);
}

function testconvolve_tiled() {
    var h = 4;
    var w = 5
    var T = TypedObject.uint32.array(h,w);
    var g = new T;
    for ( var y=0 ; y < Math.min(h,w) ; y++ )
	g[y][y] = 1;
    var g2 = convolve_tiled(g);
    pgrid(g2);
}


// scatter is buggy still, see notes in samples.js

//testeq("scatter 1", 
//       Array_scatter.call([1,2,3,4,5], [2,4,6,8,10], 0, undefined),
//       [0, 0, 1, 0, 2]);

// testeq("scatter 2", 
//        Array_scatter.call([1,2,3,4,5], [2,4,6,8,10], 0, undefined, 11),
//        [0, 0, 1, 0, 2, 0, 3, 0, 4, 0, 5])

print("OK");

//

function testeq(id, a, b) {
    if (typeof a != typeof b)
	throw new Error(id + ": not same type: " + typeof a + " " + typeof b);

    if (Array.isArray(a) || TypedObject.objectType(a) instanceof TypedObject.ArrayType) {
	if (a.length != b.length)
	    throw new Error(id + ": not same length: " + a.length + " " + b.length);
	for ( var i=0 ; i < a.length ; i++ )
	    testeq(id + " @ " + i, a[i], b[i]);
	return true;
    }

    if (a === b)
	return true;
    throw new Error(id + ": not same value: " + a + " " + b);
}

function iota(n) {
    return Array.build(n, (x) => x);
}

function TO_iota(n) {
    return TypedObject.uint32.array(n).build((x) => x);
}

function TO_float64_iota(n) {
    return TypedObject.float64.array(n).build((x) => x);
}

function pgrid(g) {
    for ( var y=0 ; y < g.length ; y++ ) {
	var s = "";
	for ( var x=0 ; x < g[0].length ; x++ )
	    s += " " + g[y][x];
	print(s);
    }
}
