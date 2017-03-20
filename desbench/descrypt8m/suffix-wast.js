// For SpiderMonkey, for testing

setJitCompilerOption('wasm.test-mode', 1); // int64 input/output

// From Stinson, p 79-81.
// Plaintext, key, expected ciphertext.
//
var canonical_test_case =
  [x64('0123456789abcdef'), x64('133457799bbcdff1'), x64('85e813540f0ab405')];

function m_test_encryption() {
    return m_des_encrypt(bits(canonical_test_case[1], 64));
}

m_test_encryption();

//print(get_output());

let binary = wasmTextToBinary(get_output());
let module = new WebAssembly.Module(binary);
let instance = new WebAssembly.Instance(module);
let exports = instance.exports;
let des_encrypt = exports["des_encrypt"];

let res = des_encrypt({high:0x01234567, low:0x89abcdef});

print((res.high >>> 0).toString(16) + (res.low >>> 0).toString(16));

