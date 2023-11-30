/* TestOfRSACodec.java -- 
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

package gnu.testlet.gnu.java.security.key.rsa;

import gnu.java.security.key.IKeyPairCodec;
import gnu.java.security.key.rsa.GnuRSAPrivateKey;
import gnu.java.security.key.rsa.GnuRSAPublicKey;
import gnu.java.security.key.rsa.RSAKeyPairGenerator;
import gnu.java.security.key.rsa.RSAKeyPairPKCS8Codec;
import gnu.java.security.key.rsa.RSAKeyPairRawCodec;
import gnu.java.security.key.rsa.RSAKeyPairX509Codec;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.security.KeyPair;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.interfaces.RSAPrivateKey;
import java.security.interfaces.RSAPublicKey;
import java.util.HashMap;

/**
 * Conformance tests for the RSA key format encoding/decoding implementations.
 */
public class TestOfRSACodec implements Testlet
{
  private HashMap map;
  private RSAKeyPairGenerator kpg;
  private KeyPair kp;

  public void test(TestHarness harness)
  {
    setUp();

    testUnknownKeyPairCodec(harness);
    testKeyPairRawCodec(harness);
    testKeyPairASN1Codec(harness);
    testPrivateKeyValueOf(harness);
    testPublicKeyValueOf(harness);
  }

  private void testUnknownKeyPairCodec(TestHarness harness)
  {
    harness.checkPoint("testUnknownKeyPairCodec");

    kp = kpg.generate();

    GnuRSAPublicKey pubK = (GnuRSAPublicKey) kp.getPublic();
    try
      {
        ((GnuRSAPublicKey) pubK).getEncoded(0);
        harness.fail("Public key succeeded with unknown format ID");
      }
    catch (IllegalArgumentException x)
      {
        harness.check(true, "Recognised unknown public key format ID");
      }

    GnuRSAPrivateKey secK = (GnuRSAPrivateKey) kp.getPrivate();
    try
      {
        ((GnuRSAPrivateKey) secK).getEncoded(0);
        harness.fail("Private key succeeded with unknown format ID");
      }
    catch (IllegalArgumentException x)
      {
        harness.check(true, "Recognised unknown private key format ID");
      }
  }

  public void testKeyPairRawCodec(TestHarness harness)
  {
    harness.checkPoint("testKeyPairRawCodec");

    byte[] pk;
    IKeyPairCodec codec = new RSAKeyPairRawCodec();
    kp = kpg.generate();

    RSAPublicKey pubK = (RSAPublicKey) kp.getPublic();
    pk = ((GnuRSAPublicKey) pubK).getEncoded(IKeyPairCodec.RAW_FORMAT);
    PublicKey newPubK = codec.decodePublicKey(pk);
    harness.check(pubK.equals(newPubK),
                  "RSA public key Raw encoder/decoder test");

    RSAPrivateKey secK = (RSAPrivateKey) kp.getPrivate();
    pk = ((GnuRSAPrivateKey) secK).getEncoded(IKeyPairCodec.RAW_FORMAT);
    PrivateKey newSecK = codec.decodePrivateKey(pk);
    harness.check(secK.equals(newSecK),
                  "RSA private key Raw encoder/decoder test");
  }

  private void testKeyPairASN1Codec(TestHarness harness)
  {
    harness.checkPoint("testKeyPairASN1Codec");

    kp = kpg.generate();

    byte[] pk;

    RSAPublicKey pubK = (RSAPublicKey) kp.getPublic();
    pk = ((GnuRSAPublicKey) pubK).getEncoded(IKeyPairCodec.X509_FORMAT);
    PublicKey newPubK = new RSAKeyPairX509Codec().decodePublicKey(pk);
    harness.check(pubK.equals(newPubK),
                  "RSA public key ASN.1 encoder/decoder test");

    RSAPrivateKey secK = (RSAPrivateKey) kp.getPrivate();
    pk = ((GnuRSAPrivateKey) secK).getEncoded(IKeyPairCodec.PKCS8_FORMAT);
    PrivateKey newSecK = new RSAKeyPairPKCS8Codec().decodePrivateKey(pk);
    harness.check(secK.equals(newSecK),
                  "RSA private key ASN.1 encoder/decoder test");
  }

  public void testPublicKeyValueOf(TestHarness harness)
  {
    harness.checkPoint("testPublicKeyValueOf");

    byte[] pk;
    kp = kpg.generate();

    RSAPublicKey p1 = (RSAPublicKey) kp.getPublic();

    pk = ((GnuRSAPublicKey) p1).getEncoded(IKeyPairCodec.RAW_FORMAT);
    PublicKey p2 = GnuRSAPublicKey.valueOf(pk);
    harness.check(p1.equals(p2),
                  "RSA public key valueOf(<raw-value>) test");

    pk = ((GnuRSAPublicKey) p1).getEncoded(IKeyPairCodec.X509_FORMAT);
    PublicKey p3 = GnuRSAPublicKey.valueOf(pk);
    harness.check(p1.equals(p3),
                  "RSA public key valueOf(<x509-value>) test");
  }

  public void testPrivateKeyValueOf(TestHarness harness)
  {
    harness.checkPoint("testPrivateKeyValueOf");

    byte[] pk;
    kp = kpg.generate();

    RSAPrivateKey p1 = (RSAPrivateKey) kp.getPrivate();

    pk = ((GnuRSAPrivateKey) p1).getEncoded(IKeyPairCodec.RAW_FORMAT);
    PrivateKey p2 = GnuRSAPrivateKey.valueOf(pk);
    harness.check(p1.equals(p2),
                  "RSA private key valueOf(<raw-value>) test");

    pk = ((GnuRSAPrivateKey) p1).getEncoded(IKeyPairCodec.PKCS8_FORMAT);
    PrivateKey p3 = GnuRSAPrivateKey.valueOf(pk);
    harness.check(p1.equals(p3),
                  "RSA private key valueOf(<pkcs8-value>) test");
  }

  private void setUp()
  {
    map = new HashMap();
    map.put(RSAKeyPairGenerator.MODULUS_LENGTH, new Integer(1024));

    kpg = new RSAKeyPairGenerator();
    kpg.setup(map);
  }
}