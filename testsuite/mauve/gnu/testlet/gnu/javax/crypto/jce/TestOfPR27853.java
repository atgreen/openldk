/* TestOfPR27853.java
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

import gnu.java.security.key.rsa.GnuRSAPrivateKey;
import gnu.java.security.key.rsa.GnuRSAPublicKey;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.math.BigInteger;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.util.Arrays;

import javax.crypto.Cipher;

/**
 * Regression test for PR Classpath/27853
 */
public class TestOfPR27853
    implements Testlet
{
  private static final BigInteger n = new BigInteger(
      "89a95ddd60ebb60c605160a1e3c8f3294614bc16e79ded40313074cde36fdc6bd2"
      + "abcd23de345de1afb97319884836f4d841dc4b8fb20c01be4a7176493ef02c02"
      + "44ab3e5f59fc0fc175a929a397641e4558847f5f2e02059a383cc5115697e2ac"
      + "018cda49ba79256e86cb0df8ba6e0bd3d4a07a8de26e5abde6b8fbf4af0be5", 16);
  private static final BigInteger e = new BigInteger("10001", 16);
  private static final BigInteger d = new BigInteger(
      "84220e17a4a4faeb6c341015b3e73906ffde7d1f5b182a16b860336d400629c350c"
      + "658b439df77d15d731ab88228169ff3475c2526fb162d4232802fb26477efbe9e"
      + "7a82cad1732aa2c0cc8196ff1c1426e047d9d028e93d3259991d82c689b10501a"
      + "46c04e0b70564143a44a1071bf2ac609a42ec9f9a4debb63802156ddc01", 16);
  private static final BigInteger p = new BigInteger(
      "8cc40f5197fbc656f3f299f0d9f0c6f6b0a31d8e893ea1050c179763f1faac4aba6"
      + "c45caf11e0a7ef4b51805877264709565692e2e1422a9344a962f6b72e8e5", 16);
  private static final BigInteger q = new BigInteger(
      "fa5ac0bbcdb4143ea94d7d68d5f45d24718235112c43e63fe04f792cb4c463cbbf4"
      + "9b70ca37c348b3047471419cd3d9ade3f65307c4c9ae3f2add3e766e86701", 16);
  private static final BigInteger dP = new BigInteger(
      "8b997c9fac9c52acb52d692184e1d64f9c09882c6d4ba12082477b29f1366a5b89d"
      + "a0ab522be6a2651c4aed7fce5a35a4baed0caad83e683eb89f4bb7e51ed49", 16);
  private static final BigInteger dQ = new BigInteger(
      "7d63a6d4691aa06921f2a5b53433c7d2d0e71e1d13c68e33bfed0e0bce1deebdc57"
      + "8ee2d6e546f1ca7798ba80da4360eb2f19d84c33cbaf7203cdfbd2e558801", 16);
  private static final BigInteger qInv = new BigInteger(
      "38be0ebc1cb06917cbcb3457f21dd8e2956eaf1d14039e845a0bed3adb1de5e5f6e"
      + "cb1da999fe408f441e635697ae47073cc6507105836c18c37b0aa49ab01e", 16);
  private static final String GNU = "GNU-CRYPTO";
  private static String transformation = "RSA";
  private static PublicKey publicK;
  private static PrivateKey privateK;
  private static Cipher cipher;

  /* (non-Javadoc)
   * @see gnu.testlet.Testlet#test(gnu.testlet.TestHarness)
   */
  public void test(TestHarness harness)
  {
    harness.checkPoint("TestOfPR27853");
    try
      {
        setUp();
        byte[] pt1 = "Does this work?".getBytes("US-ASCII");
        byte[] ct = encrypt(pt1);
        byte[] pt2 = decrypt(ct);
        harness.check(Arrays.equals(pt1, pt2), "Plain texts MUST be equal");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("test(): " + String.valueOf(x));
      }
  }

  private void setUp() throws Exception
  {
    publicK = new GnuRSAPublicKey(1, n, e);
    privateK = new GnuRSAPrivateKey(1, n, e, d, p, q, dP, dQ, qInv);
    cipher = Cipher.getInstance(transformation, GNU);
  }

  private byte[] encrypt(byte[] plainText) throws Exception
  {
    cipher.init(Cipher.ENCRYPT_MODE, publicK);
    byte[] result = cipher.doFinal(plainText);
    return result;
  }

  private byte[] decrypt(byte[] cipherText) throws Exception
  {
    cipher.init(Cipher.DECRYPT_MODE, privateK);
    byte[] result = cipher.doFinal(cipherText);
    return result;
  }
}
