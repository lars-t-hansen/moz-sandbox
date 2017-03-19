// Partial evaluation code, generating Wasm binary.
// Not yet functional.
//
// In this setup, a name may have an /address/ if it is a constant in global
// memory, or it may have a /local number/ if it is a local variable.  We
// invariably assign values to locals, and we use fresh locals for everything.
// An optimizing compiler should do well.
//
// The module can be instantiated with a memory, and this memory can be used
// for transfering values between the module and JS.
//
// The constants for the table are in the memory too, this is problematic maybe.
// How coordinate?
//
// Key values can be in globals, though that hardly makes them secure, so why
// bother.

var longlong = true;
var omit_encryption = false;

// Function parts
let m_$procname = false;
let m_$decl = [];
let m_$locals = 1;		// start at 1 because local0 = parameter
let m_$body = [];

// Global data parts
let m_$address = 0;
let m_$data = [];
let m_$data_comment = [];

var $output = "";

function display(x) {
    $output += String(x);
}

function newline() {
    $output += "\n";
}

function get_output() {
    return $output;
}

function m_init() {
    m_$procname = false;
    m_$decl = [];
    m_$locals = 1;		// see above
    m_$body = [];

    m_$address = 0;
    m_$data = [];
    m_$data_comment = [];
}

function m_exit() {
    display("(module\n");
    display("(memory 1)\n");

    for ( let [a,n] of m_$data_comment ) {
	display(format(";; ~a\t~a\n", a, n));
    }

    let d = "";
    for ( let x of m_$data ) {
	let v = unbits(x).toString(16);
	for ( let i=14 ; i >= 0 ; i-=2 )
	    d += "\\\\" + v.substring(i, i+2);
    }
    display("(data (i32.const 0) \"" + d + "\")\n")

    // Function head
    display("(func (param i64) (result i64)\n");

    // Function locals
    let i = 0;
    let k = 0;
    for ( let d of m_$decl ) {
	display("(local i64) ");
	if ((++k % 8) == 0)
	    newline();
    }
    newline();

    // Function body
    for ( let x of m_$body ) {
	display(x);
	newline();
    }

    display(format("(export \"~a\" 0))\n", m_$procname));
}

function emit(fmt, ...rest) {
    rest.unshift(fmt);
    let s = m_format.apply(null, rest);
    m_$body.push(s);
}

function m_format(fmt, ...rest) {
    let formatted = rest.map(function (x) {
	if (typeof x == "string")
	    return x;
	if (typeof x == "number")
	    return format("(i64.const ~a)", String(x));
	if (x instanceof Name) {
	    if (x.index !== undefined)
		return format("(get_local ~a)", name_index(x));
	    return format("(i64.load (i32.const ~a))", name_address(x));
	}
	if (x instanceof Array)
	    return format("(i64.const 0x~a)", strip_zeroes(unbits(x).toString(16)));
	if (x instanceof Num)
	    return format("(i64.const ~a)", x.toString());
	print("Bad value: " + x);
	(false)();
    });
    formatted.unshift(fmt);
    return format.apply(null, formatted);
}

function emit_declare_types() {
}

function emit_begin_function(procedure_name, param_name) {
    m_$procname = procedure_name;
}

function emit_end_function(return_value) {
    emit("~a)", return_value);
}

function emit_named_value(name_template, value) {
    let address = m_$address;
    m_$data_comment.push([address, name_template]);
    m_$data.push(value);
    m_$address += 8;
    return new Name(name_template, undefined, address);
}

function emit_table(name_template, length, values) {
    let address = m_$address;
    m_$data_comment.push([address, name_template]);
    for ( let i=0 ; i < length ; i++ )
	m_$data.push(values[i]);
    m_$address += length * 8;
    return new Name(name_template, undefined, address);
}

function strip_zeroes(s) {
    let l = s.length;
    let i = 0;
    while (i < l-1 && s.charAt(i) == '0')
	i++;
    return s.substring(i);
}

function m_bitmask(n) {
    let v = make_Array(64, 0);
    for ( let i=0 ; i < n ; i++ )
	v[i] = 1;
    return new Num(v);
}

function Name(id, index, address) {
    this.id = id;
    this.index = index;
    this.address = address;
}
Name.prototype.toString = function () {
    return "#<" + this.id + " " + (this.index !== undefined ? ("local " + this.index) : ("address " + this.address)) + ">";
}

function new_local(n) {
    let name = new Name(n, m_$locals++, undefined);
    m_$decl.push(name);
    return name;
}

function m_param1() {
    return new Name("text", 0, undefined);
}

function name_id(x) {
    return x.id;
}

function name_address(x) {
    return String(x.address);
}

function name_index(x) {
    return String(x.index);
}

function m_make_bitvector(length, initial) {
    let n = new_local("x");
    emit("(set_local ~a ~a)", name_index(n), initial == 0 ? 0 : -1);
    return n;
}

function m_shr_vec(v, n) {
    let x = new_local("x");
    emit("(set_local ~a (i64.shr_u ~a ~a))", name_index(x), v, n);
    return x;
}

function m_shl_vec(v, n) {
    let x = new_local("x");
    emit("(set_local ~a (i64.shl ~a ~a))", name_index(x), v, n);
    return x;
}

function m_or_vec_inplace(v1, v2) {
    emit("(set_local ~a (i64.or ~a ~a))", name_index(v1), v1, v2);
    return v1;
}

function m_and_vec(a, b) {
    let x = new_local("x");
    emit("(set_local ~a (i64.and ~a ~a))", name_index(x), a, b);
    return x;
}

function m_adjust_right(a, n) {
    let x = new_local("x");
    emit("(set_local ~a (i64.and ~a ~a))", name_index(x), a, m_bitmask(n));
    return x;
}

function m_or_vec(a, ...rest) {
    let x = new_local("x");
    let s = m_format("~a", a);
    for ( let v of rest )
	s = m_format("(i64.or ~a ~a)", s, v);
    emit("(set_local ~a ~a)", name_index(x), s);
    return x;
}

function m_unbits_vec(v) {
    return v;
}

function m_xor_vec_inplace(a, b) {
    emit("(set_local ~a (i64.xor ~a ~a))", name_index(a), a, b);
    return a;
}

function m_vector_ref(v, n) {
    let x = new_local("x");
    emit("(set_local ~a (i64.load (i32.add (i32.const ~a) (i32.shl (i32.wrap/i64 ~a) (i32.const 3)))))", name_index(x), name_address(v), n);
    return x;
}

function m_xor_vec(a, b) {
    let x = new_local("x");
    emit("(set_local ~a (i64.xor ~a ~a))", name_index(x), a, b);
    return x;
}

function m_trunc_vec(v, n) {
    return m_and_vec(v, m_bitmask(n));
}
