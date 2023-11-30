/* TestOfDSSCodec.java
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

package gnu.testlet.gnu.java.security.key.dss;

import gnu.java.security.key.IKeyPairCodec;
import gnu.java.security.key.dss.DSSKeyPairGenerator;
import gnu.java.security.key.dss.DSSKeyPairPKCS8Codec;
import gnu.java.security.key.dss.DSSKeyPairRawCodec;
import gnu.java.security.key.dss.DSSKeyPairX509Codec;
import gnu.java.security.key.dss.DSSPrivateKey;
import gnu.java.security.key.dss.DSSPublicKey;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.security.KeyPair;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.interfaces.DSAPrivateKey;
import java.security.interfaces.DSAPublicKey;
import java.util.HashMap;

/**
 * Conformance tests for the DSS key format encoding/decoding implementation.
 */
public class TestOfDSSCodec implements Testlet
{
  private HashMap map;
  private DSSKeyPairGenerator kpg = new DSSKeyPairGenerator();
  private KeyPair kp;

  public void test(TestHarness harness)
  {
    setUp();

    testUnknownKeyPairCodec(harness);
    testKeyPairRawCodec(harness);
    testKeyPairASN1Codec(harness);
    testPublicKeyValueOf(harness);
    testPrivateKeyValueOf(harness);
  }

  private void testUnknownKeyPairCodec(TestHarness harness)
  {
    harness.checkPoint("testUnknownKeyPairCodec");

    kp = kpg.generate();

    DSAPublicKey pubK = (DSAPublicKey) kp.getPublic();
    try
      {
        ((DSSPublicKey) pubK).getEncoded(0);
        harness.fail("Public key succeeded with unknown format ID");
      }
    catch (IllegalArgumentException x)
      {
        harness.check(true, "Recognised unknown public key format ID");
      }

    DSAPrivateKey secK = (DSAPrivateKey) kp.getPrivate();
    try
      {
        ((DSSPrivateKey) secK).getEncoded(0);
        harness.fail("Private key succeeded with unknown format ID");
      }
    catch (IllegalArgumentException x)
      {
        harness.check(true, "Recognised unknown private key format ID");
      }
  }

  private void testKeyPairRawCodec(TestHarness harness)
  {
    harness.checkPoint("testKeyPairRawCodec");

    kp = kpg.generate();

    IKeyPairCodec codec = new DSSKeyPairRawCodec();
    byte[] pk;

    DSAPublicKey pubK = (DSAPublicKey) kp.getPublic();
    pk = ((DSSPublicKey) pubK).getEncoded(IKeyPairCodec.RAW_FORMAT);
    PublicKey newPubK = codec.decodePublicKey(pk);
    harness.check(pubK.equals(newPubK),
                  "DSS public key Raw encoder/decoder test");

    DSAPrivateKey secK = (DSAPrivateKey) kp.getPrivate();
    pk = ((DSSPrivateKey) secK).getEncoded(IKeyPairCodec.RAW_FORMAT);
    PrivateKey newSecK = codec.decodePrivateKey(pk);
    harness.check(secK.equals(newSecK),
                  "DSS private key Raw encoder/decoder test");
  }

  private void testKeyPairASN1Codec(TestHarness harness)
  {
    harness.checkPoint("testKeyPairASN1Codec");

    kp = kpg.generate();

    byte[] pk;

    DSAPublicKey pubK = (DSAPublicKey) kp.getPublic();
    DSAPrivateKey secK = (DSAPrivateKey) kp.getPrivate();

    pk = ((DSSPrivateKey) secK).getEncoded(IKeyPairCodec.PKCS8_FORMAT);
    PrivateKey newSecK = new DSSKeyPairPKCS8Codec().decodePrivateKey(pk);
    harness.check(secK.equals(newSecK),
                  "DSS private key ASN.1 encoder/decoder test");

    pk = ((DSSPublicKey) pubK).getEncoded(IKeyPairCodec.X509_FORMAT);
    PublicKey newPubK = new DSSKeyPairX509Codec().decodePublicKey(pk);
    harness.check(pubK.equals(newPubK),
                  "DSS public key ASN.1 encoder/decoder test");
  }

  private void testPublicKeyValueOf(TestHarness harness)
  {
    harness.checkPoint("testPublicKeyValueOf");

    kp = kpg.generate();
    byte[] pk;

    DSAPublicKey p1 = (DSAPublicKey) kp.getPublic();

    pk = ((DSSPublicKey) p1).getEncoded(IKeyPairCodec.RAW_FORMAT);
    PublicKey p2 = DSSPublicKey.valueOf(pk);
    harness.check(p1.equals(p2),
                  "DSS public key valueOf(<raw-value>) test");

    pk = ((DSSPublicKey) p1).getEncoded(IKeyPairCodec.X509_FORMAT);
    PublicKey p3 = DSSPublicKey.valueOf(pk);
    harness.check(p1.equals(p3),
                  "DSS public key valueOf(<x509-value>) test");
  }

  private void testPrivateKeyValueOf(TestHarness harness)
  {
    harness.checkPoint("testPrivateKeyValueOf");

    kp = kpg.generate();
    byte[] pk;

    DSAPrivateKey p1 = (DSAPrivateKey) kp.getPrivate();

    pk = ((DSSPrivateKey) p1).getEncoded(IKeyPairCodec.RAW_FORMAT);
    PrivateKey p2 = DSSPrivateKey.valueOf(pk);
    harness.check(p1.equals(p2),
                  "DSS private key valueOf(<raw-value>) test");

    pk = ((DSSPrivateKey) p1).getEncoded(IKeyPairCodec.PKCS8_FORMAT);
    PrivateKey p3 = DSSPrivateKey.valueOf(pk);
    harness.check(p1.equals(p3),
                  "DSS private key valueOf(<pkcs8-value>) test");
  }

  private void setUp()
  {
    map = new HashMap();
    map.put(DSSKeyPairGenerator.MODULUS_LENGTH, new Integer(512));
    map.put(DSSKeyPairGenerator.USE_DEFAULTS, new Boolean(false));

    kpg = new DSSKeyPairGenerator();
    kpg.setup(map);
  }
}