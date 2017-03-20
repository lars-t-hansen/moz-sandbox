// From Stinson, p 79-81.
// Plaintext, key, expected ciphertext.

var canonical_test_case =
  [x64('0123456789abcdef'), x64('133457799bbcdff1'), x64('85e813540f0ab405')];

function m_test_encryption() {
    return m_des_encrypt(bits(canonical_test_case[1], 64));
}

m_test_encryption();
