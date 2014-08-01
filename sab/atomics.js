// Polyfill for some derived operations that are not implemented primitively yet

Atomics.sub =
    function (ita, idx, v) {
	var n = +v;
	do {
	    var v0 = ita[idx];
	} while (Atomics.compareExchange(ita, idx, v0, v0-n) != v0);
	return v0;
    };

Atomics.or =
    function (ita, idx, v) {
	var n = v|0;
	do {
	    var v0 = ita[idx];
	} while (Atomics.compareExchange(ita, idx, v0, v0|n) != v0);
	return v0;
    };
