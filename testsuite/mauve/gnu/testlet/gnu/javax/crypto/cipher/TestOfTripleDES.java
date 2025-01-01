/* TestOfTripleDES.java -- 
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
// Uses: BaseCipherTestCase

package gnu.testlet.gnu.javax.crypto.cipher;

import gnu.java.security.Properties;
import gnu.java.security.util.Util;
import gnu.javax.crypto.cipher.IBlockCipher;
import gnu.javax.crypto.cipher.TripleDES;
import gnu.testlet.TestHarness;

import java.security.InvalidKeyException;
import java.util.Arrays;
import java.util.HashMap;

/**
 * Conformance test for the Triple-DES cipher.
 */
public class TestOfTripleDES extends BaseCipherTestCase
{
  /*
   * Test vectors from the "Triple DES Monte Carlo (Modes) Test Sample
   * Results", from <a
   * href="http://csrc.nist.gov/cryptval/des/tripledes-vectors.zip">http://csrc.nist.gov/cryptval/des/tripledes-vectors.zip</a>.
   */

  /** The ECB encryption monte-carlo tests. */
  static final String[][] E_TV = {
      // key bytes (3 lines)
      // plain bytes        cipher bytes
      { "0123456789abcdef"
      + "0123456789abcdef"
      + "0123456789abcdef", // 1-key
        "4e6f772069732074", "6a2a19f41eca854b" },
      { "0123456789abcdef"
      + "23456789abcdef01"
      + "0123456789abcdef", // 2-key
       "4e6f772069732074", "03e69f5bfa58eb42" },
      { "0123456789abcdef"
      + "23456789abcdef01"
      + "456789abcdef0123", // 3-key
        "4e6f772069732074", "dd17e8b8b437d232" },
      { "6b085d92976149a4"
      + "6b085d92976149a4"
      + "6b085d92976149a4", // 1-key
        "6a2a19f41eca854b", "ce5d6c7b63177c18" },
      { "02c4da3d73f226ad"
      + "1cbce0f2bacd3b15"
      + "02c4da3d73f226ad", // 2-key
        "03e69f5bfa58eb42", "262a60f9743e1fd8" },
      { "dc34addf3d9d1fdc"
      + "976d456702cef4fd"
      + "ad49c2ba0b2f975b", // 3-key
        "dd17e8b8b437d232", "3145bcfc1c19382f" } };

  /** The ECB decryption monte-carlo tests. */
  static final String[][] D_TV = {
      { "0123456789abcdef"
      + "0123456789abcdef"
      + "0123456789abcdef", // 1-key
        "4e6f772069732074", "cdd64f2f9427c15d" },
      { "0123456789abcdef"
      + "23456789abcdef01"
      + "0123456789abcdef", // 2-key
        "4e6f772069732074", "6996c8fa47a2abeb" },
      { "0123456789abcdef"
      + "23456789abcdef01"
      + "456789abcdef0123", // 3-key
        "4e6f772069732074", "8325397644091a0a" },
      { "cdf40b491c8c0db3"
      + "cdf40b491c8c0db3"
      + "cdf40b491c8c0db3", // 1-key
        "cdd64f2f9427c15d", "5bb675e3db3a7f3b" },
      { "68b58c9dce086704"
      + "529dce3719e9e0da"
      + "68b58c9dce086704", // 2-key
        "6996c8fa47a2abeb", "6b177e016e6ae12d" },
      { "83077c10cda2d6e5"
      + "296240fd8c834fcd"
      + "8fdac4fbe5ae978f", // 3-key
        "8325397644091a0a", "c67901abdc008c89" } };

  /** The attributes map we'll use to setup the cipher. */
  private HashMap attributes = new HashMap();

  public void test(TestHarness harness)
  {
    harness.checkPoint("TestOfTripleDES");
    cipher = new TripleDES();
    HashMap attrib = new HashMap();
    attrib.put(IBlockCipher.CIPHER_BLOCK_SIZE, new Integer(8));
    attrib.put(IBlockCipher.KEY_MATERIAL, new byte[24]);

    boolean oldCheckForWeakKeys = Properties.checkForWeakKeys();
    try
      {
        Properties.setCheckForWeakKeys(false);

        harness.check(validityTest(), "validityTest()");
        harness.check(cloneabilityTest(), "cloneabilityTest()");
        harness.check(vectorsTest(), "vectorsTest()");
        harness.check(des1KeyTest(), "des1KeyTest()");
        harness.check(des2KeyTest(), "des2KeyTest()");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfTripleDES");
      }
    finally
      { // return it to its previous value
        Properties.setCheckForWeakKeys(oldCheckForWeakKeys);
      }
  }

  /** Test cloneability. */
  protected boolean cloneabilityTest() throws Exception
  {
    int blockSize = cipher.defaultBlockSize();
    int keySize = cipher.defaultKeySize();

    byte[] pt = new byte[blockSize];
    byte[] ct1 = new byte[blockSize];
    byte[] ct2 = new byte[blockSize];
    byte[] kb = new byte[keySize];
    HashMap attributes = new HashMap();
    attributes.put(IBlockCipher.KEY_MATERIAL, kb);

    cipher.reset();
    cipher.init(attributes);

    cipher.encryptBlock(pt, 0, pt, 0);
    IBlockCipher thomas = (IBlockCipher) cipher.clone();
    thomas.init(attributes);
    cipher.encryptBlock(pt, 0, ct1, 0);
    thomas.encryptBlock(pt, 0, ct2, 0);

    return Arrays.equals(ct1, ct2);
  }

  protected boolean vectorsTest() throws Exception
  {
    for (int i = 0; i < E_TV.length; i++)
      if (! encryptTest(E_TV[i][0], E_TV[i][1], E_TV[i][2]))
        return false;

    for (int i = 0; i < D_TV.length; i++)
      if (! decryptTest(D_TV[i][0], D_TV[i][1], D_TV[i][2]))
        return false;

    return true;
  }

  private boolean des1KeyTest() throws Exception
  {
    if (! encryptTest(E_TV[0][0].substring(0, 16), E_TV[0][1], E_TV[0][2]))
      return false;
    if (! encryptTest(E_TV[3][0].substring(0, 16), E_TV[3][1], E_TV[3][2]))
      return false;

    if (! decryptTest(D_TV[0][0].substring(0, 16), D_TV[0][1], D_TV[0][2]))
      return false;
    if (! decryptTest(D_TV[3][0].substring(0, 16), D_TV[3][1], D_TV[3][2]))
      return false;

    return true;
  }

  private boolean des2KeyTest() throws Exception
  {
    if (! encryptTest(E_TV[1][0].substring(0, 32), E_TV[1][1], E_TV[1][2]))
      return false;
    if (! encryptTest(E_TV[4][0].substring(0, 32), E_TV[4][1], E_TV[4][2]))
      return false;

    if (! decryptTest(D_TV[1][0].substring(0, 32), D_TV[1][1], D_TV[1][2]))
      return false;
    if (! decryptTest(D_TV[4][0].substring(0, 32), D_TV[4][1], D_TV[4][2]))
      return false;

    return true;
  }

  private boolean encryptTest(String key, String plainText, String cipherText)
      throws InvalidKeyException, IllegalStateException
  {
    attributes.clear();
    byte[] kb = Util.toBytesFromString(key);
    byte[] pt = Util.toBytesFromString(plainText);
    byte[] ct1 = Util.toBytesFromString(cipherText);
    attributes.put(IBlockCipher.KEY_MATERIAL, kb);
    cipher.reset();
    cipher.init(attributes);
    byte[] ct2 = new byte[8]; // 3-DES block size
    cipher.encryptBlock(pt, 0, ct2, 0);
    for (int j = 0; j < 9999; j++)
      cipher.encryptBlock(ct2, 0, ct2, 0);

    return Arrays.equals(ct1, ct2);
  }

  private boolean decryptTest(String key, String plainText, String cipherText)
      throws InvalidKeyException, IllegalStateException
  {
    attributes.clear();
    byte[] kb = Util.toBytesFromString(key);
    byte[] pt = Util.toBytesFromString(plainText);
    byte[] ct1 = Util.toBytesFromString(cipherText);
    attributes.put(IBlockCipher.KEY_MATERIAL, kb);
    cipher.reset();
    cipher.init(attributes);
    byte[] ct2 = new byte[8]; // 3-DES block size
    cipher.decryptBlock(pt, 0, ct2, 0);
    for (int j = 0; j < 9999; j++)
      cipher.decryptBlock(ct2, 0, ct2, 0);

    return Arrays.equals(ct1, ct2);
  }
}