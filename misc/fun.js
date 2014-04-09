function transfer(g) {
    print(g());
}

function test() {
    function g() {
	return t;
    }
    for ( var i=0 ; i < 2 ; i++ ) {
	const t = i;
	transfer(g);
    }
}

test();
