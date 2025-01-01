/* TestOfDHCodec.java -- 
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

package gnu.testlet.gnu.javax.crypto.key.dh;

import gnu.java.security.key.IKeyPairCodec;
import gnu.javax.crypto.key.dh.DHKeyPairPKCS8Codec;
import gnu.javax.crypto.key.dh.DHKeyPairRawCodec;
import gnu.javax.crypto.key.dh.DHKeyPairX509Codec;
import gnu.javax.crypto.key.dh.GnuDHKeyPairGenerator;
import gnu.javax.crypto.key.dh.GnuDHPrivateKey;
import gnu.javax.crypto.key.dh.GnuDHPublicKey;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.security.KeyPair;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.util.HashMap;

import javax.crypto.interfaces.DHPrivateKey;
import javax.crypto.interfaces.DHPublicKey;

/**
 * Conformance tests for the Diffie-Hellman key format encoding/decoding
 * implementation.
 */
public class TestOfDHCodec implements Testlet
{
  private HashMap map;
  private GnuDHKeyPairGenerator kpg = new GnuDHKeyPairGenerator();
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

    DHPublicKey pubK = (DHPublicKey) kp.getPublic();
    try
      {
        ((GnuDHPublicKey) pubK).getEncoded(0);
        harness.fail("Public key succeeded with unknown format ID");
      }
    catch (IllegalArgumentException x)
      {
        harness.check(true, "Recognised unknown public key format ID");
      }

    DHPrivateKey secK = (DHPrivateKey) kp.getPrivate();
    try
      {
        ((GnuDHPrivateKey) secK).getEncoded(0);
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

    kp = kpg.generate();

    GnuDHPublicKey pubK = (GnuDHPublicKey) kp.getPublic();
    GnuDHPrivateKey secK = (GnuDHPrivateKey) kp.getPrivate();

    byte[] pk1, pk2;

    pk1 = ((GnuDHPublicKey) pubK).getEncoded(IKeyPairCodec.RAW_FORMAT);
    pk2 = ((GnuDHPrivateKey) secK).getEncoded(IKeyPairCodec.RAW_FORMAT);

    IKeyPairCodec codec = new DHKeyPairRawCodec();
    PublicKey newPubK = codec.decodePublicKey(pk1);
    PrivateKey newSecK = codec.decodePrivateKey(pk2);

    harness.check(pubK.equals(newPubK),
                  "DH public key Raw encoder/decoder test");
    harness.check(secK.equals(newSecK),
                  "DH private key Raw encoder/decoder test");
  }

  private void testKeyPairASN1Codec(TestHarness harness)
  {
    harness.checkPoint("testKeyPairASN1Codec");

    kp = kpg.generate();

    byte[] pk;

    DHPublicKey pubK = (DHPublicKey) kp.getPublic();
    DHPrivateKey secK = (DHPrivateKey) kp.getPrivate();

    pk = ((GnuDHPrivateKey) secK).getEncoded(IKeyPairCodec.PKCS8_FORMAT);
    PrivateKey newSecK = new DHKeyPairPKCS8Codec().decodePrivateKey(pk);
    harness.check(secK.equals(newSecK),
                  "DH private key ASN.1 encoder/decoder test");

    pk = ((GnuDHPublicKey) pubK).getEncoded(IKeyPairCodec.X509_FORMAT);
    PublicKey newPubK = new DHKeyPairX509Codec().decodePublicKey(pk);
    harness.check(pubK.equals(newPubK),
                  "DH public key ASN.1 encoder/decoder test");
  }

  public void testPublicKeyValueOf(TestHarness harness)
  {
    harness.checkPoint("testPublicKeyValueOf");

    kp = kpg.generate();

    GnuDHPublicKey pubK = (GnuDHPublicKey) kp.getPublic();

    byte[] pk = ((GnuDHPublicKey) pubK).getEncoded(IKeyPairCodec.RAW_FORMAT);
    PublicKey newPubK = GnuDHPublicKey.valueOf(pk);

    harness.check(pubK.equals(newPubK),
                  "DH public key valueOf(<raw-value>) test");
  }

  public void testPrivateKeyValueOf(TestHarness harness)
  {
    harness.checkPoint("testPrivateKeyValueOf");

    kp = kpg.generate();

    GnuDHPrivateKey privateK = (GnuDHPrivateKey) kp.getPrivate();

    byte[] pk = ((GnuDHPrivateKey) privateK).getEncoded(IKeyPairCodec.RAW_FORMAT);
    PrivateKey newSecK = GnuDHPrivateKey.valueOf(pk);

    harness.check(privateK.equals(newSecK),
                  "DH public key valueOf(<raw-value>) test");
  }

  private void setUp()
  {
    map = new HashMap();
    map.put(GnuDHKeyPairGenerator.PRIME_SIZE, new Integer(512));
    map.put(GnuDHKeyPairGenerator.EXPONENT_SIZE, new Integer(160));

    kpg = new GnuDHKeyPairGenerator();
    kpg.setup(map);
  }
}