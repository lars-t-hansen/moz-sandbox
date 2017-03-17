// Partial evaluation code

var longlong = true;
var omit_encryption = false;

let m_$head = false;
let m_$body = [];
let m_$decl = [];
let m_$i = 0;

function m_init() {
  m_$head = false;
  m_$body = [];
  m_$decl = [];
  m_$i = 0;
}

function m_exit() {
    display(m_$head);
    newline();
    let i = 0;
    display("WORD ");
    for ( let d of m_$decl ) {
	display(d);
	display(", ");
	i = (i + 1) % 15;
	if (i == 0)
	    newline();
    }
    display("dummy;");
    newline();
    for ( let x of m_$body ) {
	display(x);
	newline();
    }
}

function emit_head(fmt, ...rest) {
    rest.unshift(fmt);
    m_$head = m_format.apply(null, rest);
}

function emit(fmt, ...rest) {
    rest.unshift(fmt);
    let s = m_format.apply(null, rest);
    if (!m_$head) {
	display(s);
	newline();
    } else {
	m_$body.push(s);
    }
}

function strip_zeroes(s) {
    let l = s.length;
    let i = 0;
    while (i < l-1 && s.charAt(i) == '0')
	i++;
    return s.substring(i);
}

function m_format(fmt, ...rest) {
    let suffix = longlong ? "ULL" : "UL";
    let formatted = rest.map(function (x) {
	if (typeof x == "string")
	    return x;
	if (typeof x == "number")
	    return String(x) + suffix;
	if (is_name(x))
	    return name_id(x);
	if (x instanceof Array)
	    return "0x" + strip_zeroes(unbits(x).toString(16)) + suffix;
	if (x instanceof Num)
	    return x.toString() + suffix;
	throw "Bad value: " + x;
    });
    formatted.unshift(fmt);
    return format.apply(null, formatted);
}

// Needs to create declarations.
// Note: name can carry type!

function m_name(n) {
    m_$i++;
    let id = n + "_" + m_$i.toString(36);
    m_declare(id);
    return { is_name: true, id: id }
}

function m_spclname(n) {
    return n;
}

function m_declare(id) {
    m_$decl.push(id);
}

function is_name(x) {
    return typeof x == "object" && x.is_name;
}

function name_id(x) {
    return x.id;
}

function m_make_bitvector(length, initial) {
    let n = m_name("x");
    emit("~a = ~a;", n, initial == 0 ? "0" : "~0");
    return n;
}

function m_shr_vec(v, n) {
    let x = m_name("x");
    emit("~a = ~a >> ~a;", x, v, n);
    return x;
}

function m_shl_vec(v, n) {
    let x = m_name("x");
    emit("~a = ~a << ~a;", x, v, n);
    return x;
}

function m_or_vec_inplace(v1, v2) {
    emit("~a |= ~a;", v1, v2);
    return v1;
}

function m_and_vec(a, b) {
    let x = m_name("x");
    emit("~a = ~a & ~a;", x, a, b);
    return x;
}

function m_bitmask(n) {
    let v = make_Array(64, 0);
    for ( let i=0 ; i < n ; i++ )
	v[i] = 1;
    return new Num(v);
}

function m_adjust_right(a, n) {
    let x = m_name("x");
    emit("~a = ~a & ~a;", x, a, m_bitmask(n));
    return x;
}

function m_or_vec(a, ...rest) {
    let x = m_name("x");
    emit("~a = ~a;", x, a);
    for ( let n of rest )
	emit("~a |= ~a;", x, n);
    return x;
}

function m_unbits_vec(v) {
    return v;
}

function m_xor_vec_inplace(a, b) {
    emit("~a ^= ~a;", a, b);
    return a;
}

function m_vector_ref(v, n) {
    let x = m_name("x");
    emit("~a = ~a[~a];", x, v, n);
    return x;
}

function m_xor_vec(a, b) {
    let x = m_name("x");
    emit("~a = ~a ^ ~a;", x, a, b);
    return x;
}

function m_trunc_vec(v, n) {
    return m_and_vec(v, m_bitmask(n));
}
