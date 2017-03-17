// Vector support

function make_Array(n, init) {
    let a = new Array(n);
    if (init == undefined)
	init = 0;
    if (typeof init == "function") {
	for ( let i=0 ; i < n ; i++ )
	    a[i] = init(i);
    }
    else {
	for ( let i=0 ; i < n ; i++ )
	    a[i] = init;
    }
    return a;
}

assertSame(make_Array(10, 1), [1,1,1,1,1,1,1,1,1,1]);
assertSame(make_Array(5, (x) => -x), [0,-1,-2,-3,-4]);

function permute_vec(permutation, source) {
    let l = permutation.length;
    let v = make_Array(l);
    for ( let i=0 ; i < l ; i++ )
	v[i] = source[permutation[i]];
    return v;
}

assertSame(permute_vec([3,2,1,0], [10,20,30,40]), [40,30,20,10]);

function make_bitvector(l, n) {
    return make_Array(l, n);
}
