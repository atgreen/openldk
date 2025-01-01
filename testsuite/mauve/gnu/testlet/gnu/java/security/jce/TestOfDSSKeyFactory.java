/* TestOfDSSKeyFactory.java
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

import java.security.KeyFactory;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.Security;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.PKCS8EncodedKeySpec;
import java.security.spec.X509EncodedKeySpec;

import gnu.java.security.Registry;
import gnu.java.security.provider.Gnu;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Conformance tests for the JCE DSS key-factory
 */
public class TestOfDSSKeyFactory
    implements Testlet
{
  private KeyPairGenerator kpg;
  private PublicKey publicK;
  private PrivateKey privateK;
  private KeyFactory keyFactory;
  private byte[] encoded;

  public void test(TestHarness harness)
  {
    setUp(harness);

    testSymmetry(harness);
  }

  private void setUp(TestHarness harness)
  {
    Security.addProvider(new Gnu());
    try
      {
        kpg = KeyPairGenerator.getInstance(Registry.DSS_KPG,
                                           Registry.GNU_SECURITY);
        kpg.initialize(512);
        harness.verbose("*** Generating a DSS key-pair");
        KeyPair kp = kpg.generateKeyPair();
        publicK = kp.getPublic();
        privateK = kp.getPrivate();
        keyFactory = KeyFactory.getInstance(Registry.DSS_KPG,
                                            Registry.GNU_SECURITY);
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
        encoded = publicK.getEncoded();
        X509EncodedKeySpec spec1 = new X509EncodedKeySpec(encoded);
        PublicKey k1 = keyFactory.generatePublic(spec1);
        harness.check(k1.equals(publicK), "Public DSS keys MUST be equal");

        encoded = privateK.getEncoded();
        PKCS8EncodedKeySpec spec2 = new PKCS8EncodedKeySpec(encoded);
        PrivateKey k2 = keyFactory.generatePrivate(spec2);
        harness.check(k2.equals(privateK), "Private DSS keys MUST be equal");
      }
    catch (InvalidKeySpecException x)
      {
        harness.debug(x);
        harness.fail("testSymmetry(): " + x.getMessage());
      }
  }
}
