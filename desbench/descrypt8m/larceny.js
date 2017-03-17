// Misc utilities

function assertSame(a, b) {
    if (!compareSame(a, b))
	throw new Error("Not the same:\n  " + a.toSource() + "\n  " + b.toSource());
}

function compareSame(a, b) {
    if (a == b)
	return true;

    if (a instanceof Array && b instanceof Array && a.length == b.length) {
	for ( let i=0 ; i < a.length ; i++ ) {
	    if (!compareSame(a[i], b[i]))
		return false;
	}
	return true;
    }

    return false;
}

let $output = "";

function display(x) {
    $output += String(x);
}

function newline() {
    print($output);		// Or place in an accumulator
    $output = "";
}

function format(fmt, ...args) {
    let s = "";
    let k = 0;
    for ( let i=0 ; i < fmt.length ; ) {
	let c = fmt.charAt(i++);
	if (c == '~') {
	    let d = fmt.charAt(i++);
	    if (d == 'a') {
		s += String(args[k++]);
	    } else {
		throw "Bad format char: " + d;
	    }
	} else {
	    s += c;
	}
    }
    return s;
}

assertSame(format("hi~aho", 33), "hi33ho");

function print_(fmt, ...args) {
    args.unshift(fmt);
    display(format.apply(null, args));
}

function random_integer_in_interval(low, high) {
    return low + Math.floor(Math.random() * (high - low + 1));
}
