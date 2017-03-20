
// For example:
//
// function m_test_encryption() {
//     return m_des_encrypt(bits(canonical_test_case[1], 64));
// }
//
// From Stinson, p 79-81.
// Plaintext, key, expected ciphertext.
//
// var canonical_test_case =
//   [x64('0123456789abcdef'), x64('133457799bbcdff1'), x64('85e813540f0ab405')];


// Input : 64 bits of key (with parity)
// Output: 64 bits of ciphertext

function m_des_encrypt(key_bits) {
    let new_key_schedule = key_schedule.map((l) => zero_based(l));
    m_init();
    m_des_process("des_encrypt", key_bits, new_key_schedule);
    m_exit();
}

// Input : 64 bits of key (with parity)
// Output: 64 bits of plaintext
//
// TODO: Is this actually correct? Should it not use a different key schedule?

function m_des_decrypt(key_bits) {
    let new_key_schedule = key_schedule.map((l) => zero_based(l));
    m_init();
    m_des_process("des_decrypt", key_bits, new_key_schedule);
    m_exit();
}

// S-box recomputation: produce a vector of length 64 indexed by the full
// 6 bits output from the xor in f().  Each element of the s-box is a
// bit vector of length 32 with the output bits in their proper places
// for C followed by the permutation P.

function compute_s_boxes(s_boxes) {

    var p_inverse_m = (function (old_p_m) {
	let v = make_Array(32, 0);
	let p_m = zero_based(old_p_m);
	for ( let i=0 ; i < 32 ; i++ )
	    v[p_m[i]] = i;
	return v;
    })(p_m);

    function make_mask(bits, round) {
	let roffset = round*4;
	let v = make_Array(32, 0);
	for ( let i=0 ; i < 4 ; i++ )
	    v[p_inverse_m[i+roffset]] = bits[i];
	return v;
    }

    function recompute_s_box(s_box, round) {
	let box = make_Array(64, 0);

	let tmp = s_box.map(function (line) {
	    return line.map(function (n) {
		return make_mask(bits(n, 4), round);
	    })
	});

	for ( let i=0 ; i < 64 ; i++ ) {
	    let row = 2 * Math.floor(i / 32) + i % 2;
	    let col = Math.floor(i / 2) % 16;
	    box[i] = tmp[row][col];
	}
	return box;
    }

    return iota(8).map((round) => recompute_s_box(s_boxes[round], round));
}

function m_compute_s_boxes(old_s_boxes) {
    let v = make_Array(8);
    let s_boxes = compute_s_boxes(old_s_boxes);

    for ( let i=0 ; i < 8 ; i++ ) {
	let values = [];
	for ( let j=0 ; j < 64 ; j++ )
	    values.push(s_boxes[i][j]);
	v[i] = emit_table("s_box_" + i, 64, values);
    }

    return v;
}

function compute_keys(key, keysched) {
    let v = make_Array(16);
    for ( let i=0 ; i < 16 ; i++ )
	v[i] = permute_vec(keysched[i], key);
    return v;
}

function m_compute_keys(key, keysched) {
    let v = make_Array(16);
    let keys = compute_keys(key, keysched);
    for ( let i=0 ; i < 16 ; i++ )
	v[i]= emit_named_value("key_" + i, keys[i]);
    return v;
}

function m_des_process(procedure_name, key_bits, key_schedule) {
    emit_declare_types();

    let new_s_boxes  = m_compute_s_boxes(s_boxes);
    let new_ip_m     = zero_based(ip_m);
    let new_ip_inverse_m = zero_based(ip_inverse_m);

    let keys = m_compute_keys(key_bits, key_schedule);
    let param_name = m_param1();

    emit_begin_function(procedure_name, param_name);
    let n = m_des_process_v(param_name, keys, new_s_boxes, new_ip_m, new_ip_inverse_m);
    emit_end_function(n);
}

function make_mask(n, b) {
    return bits(n, b);
}

function mask_2_i(i) {
    let bits = make_Array(64, 0);
    bits[i] = 1;
    return make_mask(new Num(bits), 64);
}

var mask0 = make_mask(x64('7C0000000000'), 48);
var mask1 = make_mask(x64('03F000000000'), 48);
var mask2 = make_mask(x64('000FC0000000'), 48);
var mask3 = make_mask(x64('00003F000000'), 48);
var mask4 = make_mask(x64('000000FC0000'), 48);
var mask5 = make_mask(x64('00000003F000'), 48);
var mask6 = make_mask(x64('000000000FC0'), 48);
var mask7 = make_mask(x64('00000000003E'), 48);

var mask63 = make_mask(x64('3F'), 48);

function m_des_process_v(text, keys, s_boxes, ip_m, ip_inverse_m) {

    function permute_vec(permutation, s) {
	let v = m_make_bitvector(64, 0);
	for ( let i=63 ; i >= 0 ; i-- ) {
	    let b = permutation[i];
	    let t = b > i ? m_shr_vec(s, b-i) : m_shl_vec(s, i-b);
	    m_or_vec_inplace(v, m_and_vec(t, mask_2_i(i)));
	}
	return v;
    }

    function ip_permute(v) {
	return permute_vec(ip_m, v);
    }

    function ip_unpermute(v) {
	return permute_vec(ip_inverse_m, v);
    }

    function expand(old_a) {
	let a = m_adjust_right(old_a, 48);
	return m_or_vec(m_shl_vec(a, 47),
			m_and_vec(m_shl_vec(a, 48-33), mask0),
			m_and_vec(m_shl_vec(a, 42-29), mask1),
			m_and_vec(m_shl_vec(a, 36-25), mask2),
			m_and_vec(m_shl_vec(a, 30-21), mask3),
			m_and_vec(m_shl_vec(a, 24-17), mask4),
			m_and_vec(m_shl_vec(a, 18-13), mask5),
			m_and_vec(m_shl_vec(a, 12-9),  mask6),
			m_and_vec(m_shl_vec(a, 6-5),   mask7),
			m_shr_vec(a, 31));
    }

    function six_bit_number(b, boffset) {
	return m_unbits_vec(m_and_vec(m_shr_vec(b, 48 - boffset - 6), mask63));
    }

    function s_box_process(b, boffset, s_box, r) {
	return m_xor_vec_inplace(r, m_vector_ref(s_box, six_bit_number(b, boffset)));
    }

    function f(a, round) {
	let b = m_xor_vec(expand(a), keys[round]);
	let res = m_make_bitvector(32, 0);
	for ( let i=0, j=0 ; j < 8 ; i+=6, j++ )
	    s_box_process(b, i, s_boxes[j], res);
	return res;
    }

    function rounds_loop(i, l, r) {
	if (i < 16)
	    return rounds_loop(i+1, r, m_xor_vec(l, f(r, i)));
	return m_or_vec(m_shl_vec(m_adjust_right(r, 64), 32), m_adjust_right(l, 64));
    }

    function rounds(l0r0) {
	if (!omit_encryption)
	    return rounds_loop(0, m_trunc_vec(m_shr_vec(l0r0, 32), 32), m_trunc_vec(l0r0, 32));
	return l0r0;
    }

    return ip_unpermute(rounds(ip_permute(text)));
}

