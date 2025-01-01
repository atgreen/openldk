/* TestOfSignature.java -- 
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

package gnu.testlet.gnu.java.security.jce;

import gnu.java.security.Registry;
import gnu.java.security.provider.Gnu;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.Security;
import java.security.Signature;

/**
 * Conformance tests for the JCE signature scheme implementations.<p>
 */
public class TestOfSignature implements Testlet
{
  private static final byte[] MESSAGE = "Que du magnifique...".getBytes();

  public void test(TestHarness harness)
  {
    setUp();

    testUnknownScheme(harness);
    testDSSSignatures(harness);
    testRSAPSSRawSignature(harness);
    testRSAPKCS1Signatures(harness);
  }

  /** Should fail with an unknown scheme. */
  public void testUnknownScheme(TestHarness harness)
  {
    harness.checkPoint("testUnknownScheme");
    try
      {
        Signature.getInstance("ABC", Registry.GNU_SECURITY);
        harness.fail("testUnknownScheme()");
      }
    catch (Exception x)
      {
        harness.check(true);
      }
  }

  public void testDSSSignatures(TestHarness harness)
  {
    KeyPairGenerator kpg = null;
    try
      {
        kpg = KeyPairGenerator.getInstance(Registry.DSS_KPG,
                                           Registry.GNU_SECURITY);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("Unable to get a DSS key-pair generator");
      }

    kpg.initialize(512);
    KeyPair kp = kpg.generateKeyPair();

    testSignature(harness, "DSS/RAW", Registry.GNU_SECURITY, kp);
    testSignature(harness, "SHA160withDSS", Registry.GNU_SECURITY, kp);
  }

  public void testRSAPSSRawSignature(TestHarness harness)
  {
    KeyPairGenerator kpg = null;
    try
      {
        kpg = KeyPairGenerator.getInstance(Registry.RSA_KPG,
                                           Registry.GNU_SECURITY);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("Unable to get an RSA key-pair generator");
      }

    kpg.initialize(1024);
    KeyPair kp = kpg.generateKeyPair();

    testSignature(harness, "RSA-PSS/RAW", Registry.GNU_SECURITY, kp);
  }

  private void testRSAPKCS1Signatures(TestHarness harness)
  {
    KeyPairGenerator kpg = null;
    try
      {
        kpg = KeyPairGenerator.getInstance(Registry.RSA_KPG,
                                           Registry.GNU_SECURITY);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("Unable to get an RSA key-pair generator");
      }

    kpg.initialize(1024);
    KeyPair kp = kpg.generateKeyPair();

    testSignature(harness, "MD2withRSA", Registry.GNU_SECURITY, kp);
    testSignature(harness, "MD5withRSA", Registry.GNU_SECURITY, kp);
    testSignature(harness, "SHA160withRSA", Registry.GNU_SECURITY, kp);
    testSignature(harness, "SHA256withRSA", Registry.GNU_SECURITY, kp);
    testSignature(harness, "SHA384withRSA", Registry.GNU_SECURITY, kp);
    testSignature(harness, "SHA512withRSA", Registry.GNU_SECURITY, kp);
  }

  private void testSignature(TestHarness harness, String sigName,
                             String provider,  KeyPair kp)
  {
    String msg = "Signature " + sigName + " provided by " + provider;
    try
      {
        Signature alice = Signature.getInstance(sigName, provider);
        Signature bob = Signature.getInstance(sigName, provider);

        alice.initSign(kp.getPrivate());
        alice.update(MESSAGE);
        byte[] signature = alice.sign();

        bob.initVerify(kp.getPublic());
        bob.update(MESSAGE);

        harness.check(bob.verify(signature), msg);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail(msg);
      }
  }

  private void setUp()
  {
    Security.addProvider(new Gnu()); // dynamically adds our provider
  }
}