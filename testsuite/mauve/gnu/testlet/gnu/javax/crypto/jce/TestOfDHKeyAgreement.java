/* TestOfDHKeyAgreement.java
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

package gnu.testlet.gnu.javax.crypto.jce;

import gnu.java.security.Registry;
import gnu.javax.crypto.jce.GnuCrypto;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.security.InvalidKeyException;
import java.security.KeyFactory;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.Security;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.X509EncodedKeySpec;
import java.util.Arrays;

import javax.crypto.KeyAgreement;

/**
 * Conformance tests for the implementation of the JCE Diffie-
 * Hellman key agreement.
 */
public class TestOfDHKeyAgreement
    implements Testlet
{
  private KeyPairGenerator kpg;
  private PublicKey alicePublicK, bobPublicK;
  private PrivateKey alicePrivateK, bobPrivateK;
  private KeyFactory aliceKF, bobKF;
  private KeyAgreement aliceKA, bobKA;


  public void test(TestHarness harness)
  {
    setUp(harness);

    testSymmetry(harness);
  }

  private void setUp(TestHarness harness)
  {
    Security.addProvider(new GnuCrypto());
    try
      {
        // alice and bob key-pairs
        kpg = KeyPairGenerator.getInstance(Registry.DH_KPG, Registry.GNU_CRYPTO);
        kpg.initialize(512);
        harness.verbose("*** Generating Alice's Diffie-Hellman key-pair");
        KeyPair aliceKP = kpg.generateKeyPair();
        alicePublicK = aliceKP.getPublic();
        alicePrivateK = aliceKP.getPrivate();
        harness.verbose("*** Generating Bob's Diffie-Hellman key-pair");
        KeyPair bobKP = kpg.generateKeyPair();
        bobPublicK = bobKP.getPublic();
        bobPrivateK = bobKP.getPrivate();

        // alice and bob key-factories
        aliceKF = KeyFactory.getInstance("DH", Registry.GNU_CRYPTO);
        bobKF = KeyFactory.getInstance("DH", Registry.GNU_CRYPTO);

        // alice and bob key-agreements
        aliceKA = KeyAgreement.getInstance(Registry.DH_KA, Registry.GNU_CRYPTO);
        bobKA = KeyAgreement.getInstance(Registry.DH_KA, Registry.GNU_CRYPTO);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("setUp(): " + x.getMessage());
      }
  }

  private void testSymmetry(TestHarness harness)
  {
    harness.checkPoint("testSymmetry()");

    try
      {
        aliceKA.init(alicePrivateK);
        byte[] aliceEncodedK = alicePublicK.getEncoded();

        harness.verbose("*** Alice sends Bob her encoded key...");

        X509EncodedKeySpec aliceKAtBob = new X509EncodedKeySpec(aliceEncodedK);
        PublicKey aliceK = bobKF.generatePublic(aliceKAtBob);
        bobKA.init(bobPrivateK);
        bobKA.doPhase(aliceK, true);
        byte[] bobSharedSecret = bobKA.generateSecret(); // bob ready
        byte[] bobEncodedK = bobPublicK.getEncoded();

        harness.verbose("*** Bob sends Alice his encoded key...");

        X509EncodedKeySpec bobKAtAlice = new X509EncodedKeySpec(bobEncodedK);
        PublicKey bobK = aliceKF.generatePublic(bobKAtAlice);
        aliceKA.doPhase(bobK, true);
        byte[] aliceSharedSecret = aliceKA.generateSecret(); // alice ready

        harness.check(Arrays.equals(aliceSharedSecret, bobSharedSecret),
                      "Shared secrets MUST be equal");
      }
    catch (InvalidKeyException x)
      {
        harness.debug(x);
        harness.fail("testSymmetry(): " + x.getMessage());
      }
    catch (InvalidKeySpecException x)
      {
        harness.debug(x);
        harness.fail("testSymmetry(): " + x.getMessage());
      }
  }
}
