/* TestOfRSAPSSSignature.java -- 
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
import gnu.java.security.sig.rsa.RSAPSSSignature;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.security.KeyPair;
import java.security.interfaces.RSAPrivateKey;
import java.security.interfaces.RSAPublicKey;
import java.util.HashMap;

/**
 * Conformance tests for the RSA-PSS signature generation/verification
 * implementation.
 */
public class TestOfRSAPSSSignature implements Testlet
{
  private RSAKeyPairGenerator kpg = new RSAKeyPairGenerator();

  private RSAPublicKey publicK;

  private RSAPrivateKey privateK;

  private RSAPSSSignature alice, bob;

  private byte[] message;

  public void test(TestHarness harness)
  {
    testSigWithDefaults(harness);
    testSigWithShaSalt16(harness);
    testSigWithRipeMD160Salt8(harness);
  }

  public void testSigWithDefaults(TestHarness harness)
  {
    harness.checkPoint("TestOfRSAPSSSignature.testSigWithDefaults");
    try
      {
        setUp();

        alice = new RSAPSSSignature(); // SHA + 0-octet salt
        bob = (RSAPSSSignature) alice.clone();

        message = "1 if by land, 2 if by sea...".getBytes();

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
        harness.fail("TestOfRSAPSSSignature.testSigWithDefaults");
      }
  }

  public void testSigWithShaSalt16(TestHarness harness)
  {
    harness.checkPoint("TestOfRSAPSSSignature.testSigWithShaSalt16");
    try
      {
        setUp();

        alice = new RSAPSSSignature(Registry.SHA1_HASH, 16);
        bob = (RSAPSSSignature) alice.clone();

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
        harness.fail("TestOfRSAPSSSignature.testSigWithShaSalt16");
      }
  }

  public void testSigWithRipeMD160Salt8(TestHarness harness)
  {
    harness.checkPoint("TestOfRSAPSSSignature.testSigWithRipeMD160Salt8");
    try
      {
        setUp();

        alice = new RSAPSSSignature(Registry.RIPEMD160_HASH, 8);
        bob = (RSAPSSSignature) alice.clone();

        message = "abcdefghijklmnopqrstuvwxyz0123456789".getBytes();

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
        harness.fail("TestOfRSAPSSSignature.testSigWithRipeMD160Salt8");
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