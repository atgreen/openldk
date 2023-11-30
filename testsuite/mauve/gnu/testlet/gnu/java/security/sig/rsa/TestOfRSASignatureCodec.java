/* TestOfRSASignatureCodec.java -- 
 Copyright (C) 2006 Free Software Foundation, Inc.
 This file is part of Mauve.

 Mauve is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2, or (at your option)
 any later version.

 Mauve is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with Mauve; see the file COPYING.  If not, write to the
 Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 02110-1301 USA.

 */

// Tags: GNU-CRYPTO JDK1.4

package gnu.testlet.gnu.java.security.sig.rsa;

import gnu.java.security.Registry;
import gnu.java.security.key.rsa.RSAKeyPairGenerator;
import gnu.java.security.sig.BaseSignature;
import gnu.java.security.sig.ISignature;
import gnu.java.security.sig.ISignatureCodec;
import gnu.java.security.sig.SignatureCodecFactory;
import gnu.java.security.sig.SignatureFactory;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.security.KeyPair;
import java.security.interfaces.RSAPrivateKey;
import java.security.interfaces.RSAPublicKey;
import java.util.HashMap;

/**
 * Conformance tests for the RSA signature format encoding/decoding
 * implementation.
 */
public class TestOfRSASignatureCodec implements Testlet
{
  private static final String RAW = Registry.RAW_ENCODING_SHORT_NAME;
  private static final String ASN1 = Registry.X509_ENCODING_SORT_NAME;
  private static final byte[] MESSAGE = "1 if by land, 2 if by sea...".getBytes();
  private RSAPublicKey pubK;
  private RSAPrivateKey secK;

  public void test(TestHarness harness)
  {
    setUp();

    testCodec(harness, Registry.RSA_PSS_SIG, RAW);
    testCodec(harness, Registry.RSA_PSS_SIG + "-" + Registry.MD2_HASH, RAW);
    testCodec(harness, Registry.RSA_PSS_SIG + "-" + Registry.MD5_HASH, RAW);
    testCodec(harness, Registry.RSA_PSS_SIG + "-" + Registry.SHA160_HASH, RAW);
    testCodec(harness, Registry.RSA_PSS_SIG + "-" + Registry.SHA256_HASH, RAW);
    testCodec(harness, Registry.RSA_PSS_SIG + "-" + Registry.SHA384_HASH, RAW);
    testCodec(harness, Registry.RSA_PSS_SIG + "-" + Registry.SHA512_HASH, RAW);
    testCodec(harness, Registry.RSA_PSS_SIG + "-" + Registry.RIPEMD128_HASH, RAW);
    testCodec(harness, Registry.RSA_PSS_SIG + "-" + Registry.RIPEMD160_HASH, RAW);

    testCodec(harness, Registry.RSA_PKCS1_V1_5_SIG, RAW);
    testCodec(harness, Registry.RSA_PKCS1_V1_5_SIG + "-" + Registry.MD2_HASH, RAW);
    testCodec(harness, Registry.RSA_PKCS1_V1_5_SIG + "-" + Registry.MD5_HASH, RAW);
    testCodec(harness, Registry.RSA_PKCS1_V1_5_SIG + "-" + Registry.SHA160_HASH, RAW);
    testCodec(harness, Registry.RSA_PKCS1_V1_5_SIG + "-" + Registry.SHA256_HASH, RAW);
    testCodec(harness, Registry.RSA_PKCS1_V1_5_SIG + "-" + Registry.SHA384_HASH, RAW);
    testCodec(harness, Registry.RSA_PKCS1_V1_5_SIG + "-" + Registry.SHA512_HASH, RAW);

    testCodec(harness, Registry.RSA_PKCS1_V1_5_SIG, ASN1);
    testCodec(harness, Registry.RSA_PKCS1_V1_5_SIG + "-" + Registry.MD2_HASH, ASN1);
    testCodec(harness, Registry.RSA_PKCS1_V1_5_SIG + "-" + Registry.MD5_HASH, ASN1);
    testCodec(harness, Registry.RSA_PKCS1_V1_5_SIG + "-" + Registry.SHA160_HASH, ASN1);
    testCodec(harness, Registry.RSA_PKCS1_V1_5_SIG + "-" + Registry.SHA256_HASH, ASN1);
    testCodec(harness, Registry.RSA_PKCS1_V1_5_SIG + "-" + Registry.SHA384_HASH, ASN1);
    testCodec(harness, Registry.RSA_PKCS1_V1_5_SIG + "-" + Registry.SHA512_HASH, ASN1);
  }

  private void setUp()
  {
    HashMap map = new HashMap();
    map.put(RSAKeyPairGenerator.MODULUS_LENGTH, new Integer(1024));

    RSAKeyPairGenerator kpg = new RSAKeyPairGenerator();
    kpg.setup(map);

    KeyPair kp = kpg.generate();

    pubK = (RSAPublicKey) kp.getPublic();
    secK = (RSAPrivateKey) kp.getPrivate();
  }

  private void testCodec(TestHarness harness, String sigName, String format)
  {
    harness.checkPoint("Signature codec " + sigName + "/" + format);

    ISignature alice = SignatureFactory.getInstance(sigName);
    ISignature bob = (ISignature) alice.clone();
    ISignatureCodec codec = SignatureCodecFactory.getInstance(sigName, format);

    HashMap map = new HashMap();
    map.put(BaseSignature.SIGNER_KEY, secK);
    alice.setupSign(map);
    alice.update(MESSAGE, 0, MESSAGE.length);
    Object signature = alice.sign();

    byte[] encodedSignature = codec.encodeSignature(signature);
    Object decodedSignature = codec.decodeSignature(encodedSignature);

    map.put(BaseSignature.VERIFIER_KEY, pubK);
    bob.setupVerify(map);
    bob.update(MESSAGE, 0, MESSAGE.length);

    harness.check(bob.verify(decodedSignature));
  }
}