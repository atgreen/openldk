/* TestOfRSAPKCS1V1_5Signature.java -- 
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
import gnu.java.security.sig.rsa.RSAPKCS1V1_5Signature;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.security.KeyPair;
import java.security.interfaces.RSAPrivateKey;
import java.security.interfaces.RSAPublicKey;
import java.util.HashMap;

/**
 * Conformance tests for the RSA-PKCS1-V1.5 signature generation/verification
 * implementation.
 */
public class TestOfRSAPKCS1V1_5Signature implements Testlet
{
  private RSAKeyPairGenerator kpg = new RSAKeyPairGenerator();

  private RSAPublicKey publicK;

  private RSAPrivateKey privateK;

  private RSAPKCS1V1_5Signature alice, bob;

  private byte[] message;

  public void test(TestHarness harness)
  {
    testSigWithHash(harness, Registry.MD2_HASH);
    testSigWithHash(harness, Registry.MD5_HASH);
    testSigWithHash(harness, Registry.SHA160_HASH);
    testSigWithHash(harness, Registry.SHA256_HASH);
    testSigWithHash(harness, Registry.SHA384_HASH);
    testSigWithHash(harness, Registry.SHA512_HASH);
  }

  private void testSigWithHash(TestHarness harness, String name)
  {
    harness.checkPoint("TestOfRSAPKCS1V1_5Signature.with(" + name + ")");
    try
      {
        setUp();

        alice = new RSAPKCS1V1_5Signature(name);
        bob = (RSAPKCS1V1_5Signature) alice.clone();

        message = "Que du magnifique...".getBytes();

        HashMap map = new HashMap();
        map.put(BaseSignature.SIGNER_KEY, privateK);
        alice.setupSign(map);
        alice.update(message, 0, message.length);
        Object signature = alice.sign();

        map.put(BaseSignature.VERIFIER_KEY, publicK);
        bob.setupVerify(map);
        bob.update(message, 0, message.length);

        harness.check(bob.verify(signature));
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfRSAPKCS1V1_5Signature.with(" + name + ")");
      }
  }

  private void setUp()
  {
    kpg.setup(new HashMap()); // default is to use 1024-bit keys
    KeyPair kp = kpg.generate();
    publicK = (RSAPublicKey) kp.getPublic();
    privateK = (RSAPrivateKey) kp.getPrivate();
  }
}