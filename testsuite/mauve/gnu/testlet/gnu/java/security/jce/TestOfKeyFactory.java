/* TestOfKeyFactory.java
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
import gnu.javax.crypto.jce.GnuCrypto;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.security.KeyFactory;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.Security;
import java.security.spec.DSAPrivateKeySpec;
import java.security.spec.DSAPublicKeySpec;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.KeySpec;
import java.security.spec.PKCS8EncodedKeySpec;
import java.security.spec.RSAPrivateCrtKeySpec;
import java.security.spec.RSAPublicKeySpec;
import java.security.spec.X509EncodedKeySpec;

import javax.crypto.spec.DHPrivateKeySpec;
import javax.crypto.spec.DHPublicKeySpec;

/**
 * Conformance tests for the JCE DSS and RSA key-factories.
 */
public class TestOfKeyFactory
    implements Testlet
{
  private KeyPairGenerator dssKPG;
  private KeyPairGenerator rsaKPG;
  private KeyPairGenerator dhKPG;
  private KeyFactory dssKF;
  private KeyFactory rsaKF;
  private KeyFactory dhKF;
  private KeyFactory encKF;

  public void test(TestHarness harness)
  {
    setUp(harness);

    testDSSKeyFactory(harness);
    testRSAKeyFactory(harness);
    testDHKeyFactory(harness);
  }

  private void setUp(TestHarness harness)
  {
    Security.addProvider(new Gnu());
    Security.addProvider(new GnuCrypto());
    try
      {
        dssKPG = KeyPairGenerator.getInstance(Registry.DSS_KPG,
                                              Registry.GNU_SECURITY);
        rsaKPG = KeyPairGenerator.getInstance(Registry.RSA_KPG,
                                              Registry.GNU_SECURITY);
        dhKPG = KeyPairGenerator.getInstance(Registry.DH_KPG,
                                             Registry.GNU_CRYPTO);
        dssKF = KeyFactory.getInstance(Registry.DSS_KPG, Registry.GNU_SECURITY);
        rsaKF = KeyFactory.getInstance(Registry.RSA_KPG, Registry.GNU_SECURITY);
        dhKF = KeyFactory.getInstance(Registry.DH_KPG, Registry.GNU_CRYPTO);
        encKF = KeyFactory.getInstance("Encoded", Registry.GNU_SECURITY);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("setUp(): " + x.getMessage());
      }
  }

  private void testDSSKeyFactory(TestHarness harness)
  {
    harness.checkPoint("testDSSKeyFactory");

    dssKPG.initialize(512);
    KeyPair kp = dssKPG.generateKeyPair();
    harness.check(kp != null, "MUST generate valid DSS keypair");

    PublicKey p1, p2, p3;
    KeySpec spec1, spec2;
    PrivateKey p4, p5, p6;
    String msg;

    p1 = kp.getPublic();
    try
      {
        spec1 = dssKF.getKeySpec(p1, DSAPublicKeySpec.class);
        p2 = encKF.generatePublic(spec1);
        harness.check(p2.equals(p1), "Two DSS public keys MUST be equal");

        spec2 = dssKF.getKeySpec(p1, X509EncodedKeySpec.class);
        p3 = encKF.generatePublic(spec2);
        harness.check(p3.equals(p2), "Two decoded DSS public keys MUST be equal");
      }
    catch (InvalidKeySpecException x)
      {
        harness.debug(x);
        harness.fail("Unable to generate encoded DSS public keys");
      }

    msg = "MUST NOT emit PKCS#8 encoding for DSS public key";
    try
      {
        dssKF.getKeySpec(p1, PKCS8EncodedKeySpec.class);
        harness.fail(msg);
      }
    catch (InvalidKeySpecException x)
      {
        harness.check(true, msg);
      }

    p4 = kp.getPrivate();
    try
      {
        spec1 = dssKF.getKeySpec(p4, DSAPrivateKeySpec.class);
        p5 = encKF.generatePrivate(spec1);
        harness.check(p5.equals(p4), "Two DSS private keys MUST be equal");

        spec2 = dssKF.getKeySpec(p4, PKCS8EncodedKeySpec.class);
        p6 = encKF.generatePrivate(spec2);
        harness.check(p6.equals(p5), "Two decoded DSS private keys MUST be equal");
      }
    catch (InvalidKeySpecException x)
      {
        harness.debug(x);
        harness.fail("Unable to generate encoded DSS private keys");
      }

    msg = "MUST NOT emit X.509 encoding for DSS private key";
    try
      {
        dssKF.getKeySpec(p4, X509EncodedKeySpec.class);
        harness.fail(msg);
      }
    catch (InvalidKeySpecException x)
      {
        harness.check(true, msg);
      }
  }

  private void testRSAKeyFactory(TestHarness harness)
  {
    harness.checkPoint("testRSAKeyFactory");

    rsaKPG.initialize(1024);
    KeyPair kp = rsaKPG.generateKeyPair();
    harness.check(kp != null, "MUST generate valid RSA keypair");

    PublicKey p1, p2, p3;
    KeySpec spec1, spec2;
    PrivateKey p4, p5, p6;
    String msg;

    p1 = kp.getPublic();
    try
      {
        spec1 = rsaKF.getKeySpec(p1, RSAPublicKeySpec.class);
        p2 = encKF.generatePublic(spec1);
        harness.check(p2.equals(p1), "Two RSA public keys MUST be equal");

        spec2 = rsaKF.getKeySpec(p1, X509EncodedKeySpec.class);
        p3 = encKF.generatePublic(spec2);
        harness.check(p3.equals(p2), "Two decoded RSA public keys MUST be equal");
      }
    catch (InvalidKeySpecException x)
      {
        harness.debug(x);
        harness.fail("Unable to generate encoded RSA public keys");
      }

    msg = "MUST NOT emit PKCS#8 encoding for RSA public key";
    try
      {
        rsaKF.getKeySpec(p1, PKCS8EncodedKeySpec.class);
        harness.fail(msg);
      }
    catch (InvalidKeySpecException x)
      {
        harness.check(true, msg);
      }

    p4 = kp.getPrivate();
    try
      {
        spec1 = rsaKF.getKeySpec(p4, RSAPrivateCrtKeySpec.class);
        p5 = encKF.generatePrivate(spec1);
        harness.check(p5.equals(p4), "Two RSA private keys MUST be equal");

        spec2 = rsaKF.getKeySpec(p4, PKCS8EncodedKeySpec.class);
        p6 = encKF.generatePrivate(spec2);
        harness.check(p6.equals(p5), "Two decoded RSA private keys MUST be equal");
      }
    catch (InvalidKeySpecException x)
      {
        harness.debug(x);
        harness.fail("Unable to generate encoded RSA private keys");
      }

    msg = "MUST NOT emit X.509 encoding for RSA private key";
    try
      {
        rsaKF.getKeySpec(p4, X509EncodedKeySpec.class);
        harness.fail(msg);
      }
    catch (InvalidKeySpecException x)
      {
        harness.check(true, msg);
      }
  }

  private void testDHKeyFactory(TestHarness harness)
  {
    harness.checkPoint("testDHKeyFactory");

    dhKPG.initialize(512);
    KeyPair kp = dhKPG.generateKeyPair();
    harness.check(kp != null, "MUST generate valid DH keypair");

    PublicKey p1, p2, p3;
    KeySpec spec1, spec2;
    PrivateKey p4, p5, p6;
    String msg;

    p1 = kp.getPublic();
    try
      {
        spec1 = dhKF.getKeySpec(p1, DHPublicKeySpec.class);
        p2 = encKF.generatePublic(spec1);
        harness.check(p2.equals(p1), "Two DH public keys MUST be equal");

        spec2 = dhKF.getKeySpec(p1, X509EncodedKeySpec.class);
        p3 = encKF.generatePublic(spec2);
        harness.check(p3.equals(p2), "Two decoded DH public keys MUST be equal");
      }
    catch (InvalidKeySpecException x)
      {
        harness.debug(x);
        harness.fail("Unable to generate encoded DH public keys");
      }

    msg = "MUST NOT emit PKCS#8 encoding for DH public key";
    try
      {
        dhKF.getKeySpec(p1, PKCS8EncodedKeySpec.class);
        harness.fail(msg);
      }
    catch (InvalidKeySpecException x)
      {
        harness.check(true, msg);
      }

    p4 = kp.getPrivate();
    try
      {
        spec1 = dhKF.getKeySpec(p4, DHPrivateKeySpec.class);
        p5 = encKF.generatePrivate(spec1);
        harness.check(p5.equals(p4), "Two DH private keys MUST be equal");

        spec2 = dhKF.getKeySpec(p4, PKCS8EncodedKeySpec.class);
        p6 = encKF.generatePrivate(spec2);
        harness.check(p6.equals(p5), "Two decoded DH private keys MUST be equal");
      }
    catch (InvalidKeySpecException x)
      {
        harness.debug(x);
        harness.fail("Unable to generate encoded DH private keys");
      }

    msg = "MUST NOT emit X.509 encoding for DH private key";
    try
      {
        dhKF.getKeySpec(p4, X509EncodedKeySpec.class);
        harness.fail(msg);
      }
    catch (InvalidKeySpecException x)
      {
        harness.check(true, msg);
      }
  }
}
