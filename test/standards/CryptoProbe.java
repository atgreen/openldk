// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import javax.crypto.*;
import javax.crypto.spec.*;
import java.security.*;
import java.security.spec.*;
import java.util.*;
import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;

public class CryptoProbe {
    static void check(boolean v) { if (!v) throw new AssertionError(); }
    public static void main(String[] args) throws Exception {
        var hex = HexFormat.of();
        byte[] ikm = new byte[22]; Arrays.fill(ikm, (byte) 0x0b);
        var spec = HKDFParameterSpec.ofExtract().addIKM(ikm)
            .addSalt(hex.parseHex("000102030405060708090a0b0c"))
            .thenExpand(hex.parseHex("f0f1f2f3f4f5f6f7f8f9"), 42);
        var kdf = KDF.getInstance("HKDF-SHA256");
        byte[] expected = hex.parseHex("3cb25f25faacd57a90434f64d0362f2a2d2d0a90cf1a5a4c5db02d56ecc4c5bf34007208d5b887185865");
        check(Arrays.equals(kdf.deriveData(spec), expected));
        check(Arrays.equals(kdf.deriveKey("AES", spec).getEncoded(), expected));
        var extract = HKDFParameterSpec.ofExtract().addIKM(ikm)
            .addSalt(hex.parseHex("000102030405060708090a0b0c")).extractOnly();
        byte[] prk = hex.parseHex("077709362c2e32df0ddc3f0dc47bba6390b6c73bb50f9c3122ec844ad7c2b3e5");
        check(Arrays.equals(kdf.deriveData(extract), prk));
        check(Arrays.equals(kdf.deriveData(HKDFParameterSpec.expandOnly(new SecretKeySpec(prk, "HKDF-SHA256"),
            hex.parseHex("f0f1f2f3f4f5f6f7f8f9"), 42)), expected));
        check(Arrays.equals(KDF.getInstance("HKDF-SHA256", kdf.getProviderName()).deriveData(spec), expected));
        for (String algorithm : new String[]{"HKDF-SHA384", "HKDF-SHA512"}) {
            var other = KDF.getInstance(algorithm);
            byte[] derived = other.deriveData(spec);
            check(derived.length == 42 && Arrays.equals(derived, other.deriveKey("AES", spec).getEncoded()));
            // Compare exact bytes across engines, not just self-consistency.
            System.out.println(algorithm + "=" + hex.formatHex(derived));
        }
        try { KDF.getInstance("NoSuchKDF"); throw new AssertionError(); }
        catch (NoSuchAlgorithmException expectedError) { }
        // A fixed RFC 8410 Ed25519 private-key encoding keeps comparisons deterministic.
        byte[] der = hex.parseHex("302e020100300506032b6570042204209d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60");
        var key = KeyFactory.getInstance("Ed25519").generatePrivate(new PKCS8EncodedKeySpec(der));
        String pem = PEMEncoder.of().encodeToString(key);
        check(pem.contains("BEGIN PRIVATE KEY"));
        var decoded = PEMDecoder.of().decode(pem, PrivateKey.class);
        check(Arrays.equals(decoded.getEncoded(), der));
        check(Arrays.equals(PEMDecoder.of().decode(new ByteArrayInputStream(pem.getBytes(StandardCharsets.US_ASCII)),
            PrivateKey.class).getEncoded(), der));
        String encrypted = PEMEncoder.of().withEncryption("test-password".toCharArray()).encodeToString(key);
        check(Arrays.equals(PEMDecoder.of().withDecryption("test-password".toCharArray())
            .decode(encrypted, PrivateKey.class).getEncoded(), der));
        System.out.println("Cryptography: RFC 5869 HKDF and plain/encrypted PEM passed");
    }
}
