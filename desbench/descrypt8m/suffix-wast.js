setJitCompilerOption('wasm.test-mode', 1); // int64 input/output

m_test_encryption();

//print(get_output());
let binary = wasmTextToBinary(get_output());
let module = new WebAssembly.Module(binary);
let instance = new WebAssembly.Instance(module);
let exports = instance.exports;

// possible error sources:
//
// - input interpreted differently
// - output ditto (signedness? seems unlikely)
// - memory references
// - data layout
// - incorrect formatting of hex/nonhex values in wast
// - wasm semantics differ from c in some obscure way
//
// bad output is  0e07 1d7e 46b4 3ddf
// good output is 85e8 1354 0f0a b405

let res = exports["des_encrypt"]({high:0x01234567, low:0x89abcdef});
print(res.high.toString(16) + res.low.toString(16));
