// A Num represents a 64-bit number as a vector of bits.
// LSB at lower indices.

function Num(bits) {
    assertSame(bits.length, 64);
    this.bits = bits;
}

Num.prototype.toString = function (radix) {
    let s = "";
    let k = 0;
    for ( let i=0 ; i < 16 ; i++ ) {
	let v = 0;
	for ( let j=0; j < 4; j++ )
	    v = v | (this.bits[k++] << j);
	s = "0123456789abcdef"[v] + s;
    }
    if (radix != 16)
	s = "0x" + s;
    return s;
}

assertSame((new Num([0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,
		     0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0])).toString(16), '0000000000000000');

assertSame((new Num([0,0,0,0, 1,0,0,0, 0,1,0,0, 0,0,0,0, 0,0,1,0, 1,0,1,0, 0,0,0,0, 0,0,0,1,
		     1,0,0,1, 0,0,0,0, 0,1,0,1, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0])).toString(16), '00000a0980540210');

function x64(s) {
    function hexval(x) {
	if (x >= '0' && x <= '9') return x.charCodeAt(0) - '0'.charCodeAt(0);
	if (x >= 'a' && x <= 'f') return x.charCodeAt(0) - 'a'.charCodeAt(0) + 10;
	if (x >= 'A' && x <= 'F') return x.charCodeAt(0) - 'A'.charCodeAt(0) + 10;
	throw new Error("Not a hex digit: " + x);
    }

    let v = make_Array(64, 0);
    let k = 0;
    for ( let i=s.length-1 ; i >= 0 ; --i ) {
	let val = hexval(s.charAt(i));
	for ( let j=0 ; j < 4 ; j++ ) {
	    v[k++] = val & 1;
	    val >>= 1;
	}
    }

    return new Num(v);
}

assertSame(x64('0123456789abcdef').toString(16), '0123456789abcdef');
assertSame(x64('56789abcdef').toString(16), '0000056789abcdef');
