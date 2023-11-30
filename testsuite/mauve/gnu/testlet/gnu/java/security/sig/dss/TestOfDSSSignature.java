/* TestOfDSSSignature.java -- 
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

package gnu.testlet.gnu.java.security.sig.dss;

import gnu.java.security.Registry;
import gnu.java.security.hash.IMessageDigest;
import gnu.java.security.hash.Sha160;
import gnu.java.security.key.dss.DSSPrivateKey;
import gnu.java.security.key.dss.DSSPublicKey;
import gnu.java.security.sig.BaseSignature;
import gnu.java.security.sig.dss.DSSSignature;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.math.BigInteger;
import java.security.interfaces.DSAPrivateKey;
import java.security.interfaces.DSAPublicKey;
import java.util.HashMap;

/**
 * Conformance tests for the DSS signature generation/verification
 * implementation.
 */
public class TestOfDSSSignature implements Testlet
{
  /** Common DSA key material - p. */
  private static final BigInteger p = new BigInteger(
      "fd7f53811d75122952df4a9c2eece4e7f611b7523cef4400c31e3f80b6512669455d4"
      + "02251fb593d8d58fabfc5f5ba30f6cb9b556cd7813b801d346ff26660b76b9950a5"
      + "a49f9fe8047b1022c24fbba9d7feb7c61bf83b57e7c6a8a6150f04fb83f6d3c51ec"
      + "3023554135a169132f675f3ae2b61d72aeff22203199dd14801c7", 16);
  /** Common DSA key material - q. */
  private static final BigInteger q = new BigInteger(
      "9760508f15230bccb292b982a2eb840bf0581cf5", 16);
  /** Common DSA key material - g. */
  private static final BigInteger g = new BigInteger(
      "f7e1a085d69b3ddecbbcab5c36b857b97994afbbfa3aea82f9574c0b3d07826751595"
      + "78ebad4594fe67107108180b449167123e84c281613b7cf09328cc8a6e13c167a8b"
      + "547c8d28e0a3ae1e2bb3a675916ea37f0bfa213562f1fb627a01243bcca4f1bea85"
      + "19089a883dfe15ae59f06928b665e807b552564014c3bfecf492a", 16);
  /** Public DSA key part. */
  private static final BigInteger x = new BigInteger(
      "631305a19984821b95a8c776d38167a4ea2ceb8", 16);
  /** Private DSA key part. */
  private static final BigInteger y = new BigInteger(
      "cc1045a7550205a581ec3a9fed50c6d4aaae9ef2512c066f0d52617e0d462895c00bd"
      + "f2d329c53a9c0f690e406d49e21beb557d47436df9cdda5ad2f532620a5260704c5"
      + "91920ff674666e2166066727051f3d515aedf03a4bdb2d69dd13bbd9b5e7941ff37"
      + "fb35f2d9138b4172e64393b04afdcc630739fbe6993f27f467e17", 16);

  public void test(TestHarness harness)
  {
    harness.checkPoint("TestOfDSSSignature");

    DSAPublicKey publicK = new DSSPublicKey(Registry.ASN1_ENCODING_ID, p, q, g, y);
    DSAPrivateKey privateK = new DSSPrivateKey(Registry.ASN1_ENCODING_ID, p, q, g, x);

    DSSSignature alice = new DSSSignature();
    DSSSignature bob = (DSSSignature) alice.clone();

    byte[] message = "1 if by land, 2 if by sea...".getBytes();

    HashMap map = new HashMap();
    map.put(BaseSignature.SIGNER_KEY, privateK);
    alice.setupSign(map);
    alice.update(message, 0, message.length);
    Object signature = alice.sign();

    map.put(BaseSignature.VERIFIER_KEY, publicK);
    bob.setupVerify(map);
    bob.update(message, 0, message.length);

    harness.check(bob.verify(signature), "instance methods");

    IMessageDigest sha = new Sha160();
    sha.update(message, 0, message.length);
    byte[] hash = sha.digest();
    BigInteger[] rs = DSSSignature.sign(privateK, hash);

    harness.check(DSSSignature.verify(publicK, hash, rs), "class methods");
  }
}