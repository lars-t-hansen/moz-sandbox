//let mode = "testing";
let mode = "benchmarking";

// From Stinson, p 79-81.
// Plaintext, key, expected ciphertext.
//
let canonical_test_case =
  [x64('0123456789abcdef'), x64('133457799bbcdff1'), x64('85e813540f0ab405')];

// TODO: We want some kind of generator object here: new Descrypt8m(...) that encapsulates
// globals, etc.

let input_base = 0;
let input_memory = 1024*1024;
m_set_base_address(input_base + input_memory);	 // Low megabyte reserved for input
m_des_encrypt(bits(canonical_test_case[1], 64)); // Specialize algorithm for key
//print(get_output());
let binary = wasmTextToBinary(get_output());
let module = new WebAssembly.Module(binary);
let size = Math.floor((input_base + input_memory + (m_private_memory_size()+65535))/65536);
let memory = new WebAssembly.Memory({initial:size, maximum:size});
let instance = new WebAssembly.Instance(module, {"":{mem:memory}});
let exports = instance.exports;
let des_encrypt = exports["des_encrypt"];
let base = input_base;

if (mode == "benchmarking") {
    let then = Date.now();
    for ( let i=0 ; i < 80 ; i++ )
	des_encrypt(base, base+1024*1024);
    let now = Date.now();
    print("Time for 80MB: " + (now-then)/1000 + "s");
}

if (mode == "testing") {
    let u32 = new Uint32Array(memory.buffer);
    u32[base/4]     = 0x89abcdef;
    u32[base/4 + 1] = 0x01234567;
    des_encrypt(base, base+8);
    print("Expect: " + '85e813540f0ab405')
    print("Got:    " + pad(u32[base/4 + 1].toString(16)) + pad(u32[base/4].toString(16)));
}

function pad(s) {
    while (s.length < 8)
	s = '0' + s;
    return s;
}

