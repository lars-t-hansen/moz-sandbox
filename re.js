// Fuzzer failure

print(Array.buildPar(7, function(z = d, ...x) { return { N: RegExp() } }))
