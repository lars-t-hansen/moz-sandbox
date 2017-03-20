setJitCompilerOption('wasm.test-mode', 1); // int64 input/output

m_test_encryption();

//print(get_output());

let binary = wasmTextToBinary(get_output());
let module = new WebAssembly.Module(binary);
let instance = new WebAssembly.Instance(module);
let exports = instance.exports;
let des_encrypt = exports["des_encrypt"];

let res = des_encrypt({high:0x01234567, low:0x89abcdef});

print((res.high >>> 0).toString(16) + (res.low >>> 0).toString(16));
