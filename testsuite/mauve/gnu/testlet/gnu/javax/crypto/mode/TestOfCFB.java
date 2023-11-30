/* TestOfCFB.java -- 
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

package gnu.testlet.gnu.javax.crypto.mode;

import gnu.java.security.Registry;
import gnu.java.security.util.Util;
import gnu.javax.crypto.mode.IMode;
import gnu.javax.crypto.mode.ModeFactory;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

/**
 * Conformance tests of the CFB implementation.
 *
 * <p>References:</p>
 * <ol>
 *    <li><a href="http://csrc.nist.gov/encryption/modes/Recommendation/Modes01.pdf">
 *    Recommendation for Block Cipher Modes of Operation Methods and Techniques</a>,
 *    Morris Dworkin.</li>
 * </ol>
 */
public class TestOfCFB implements Testlet
{
  private byte[] key, iv, pt, ct, pt1, pt2, pt3, pt4, pt5, pt6, pt7, pt8, pt9,
      pt10, pt11, pt12, pt13, pt14, pt15, pt16, pt17, pt18, ct1, ct2, ct3, ct4,
      ct5, ct6, ct7, ct8, ct9, ct10, ct11, ct12, ct13, ct14, ct15, ct16, ct17,
      ct18;

  private IMode mode;

  private Map attributes = new HashMap();

  public void test(TestHarness harness)
  {
    // CFB1 mode is omitted, since it is not supported in GNU Crypto.

    harness.checkPoint("TestOfCFB8.testAES128");
    /* F.3.7 CFB8-AES128-Encrypt and F.3.8 CFB8-AES-Decrypt. */
    key = Util.toBytesFromUnicode("\u2b7e\u1516\u28ae\ud2a6\uabf7\u1588\u09cf\u4f3c");
    iv = Util.toBytesFromUnicode("\u0001\u0203\u0405\u0607\u0809\u0a0b\u0c0d\u0e0f");

    pt1 = Util.toBytesFromString("6b");
    ct1 = Util.toBytesFromString("3b");

    pt2 = Util.toBytesFromString("c1");
    ct2 = Util.toBytesFromString("79");

    pt3 = Util.toBytesFromString("be");
    ct3 = Util.toBytesFromString("42");

    pt4 = Util.toBytesFromString("e2");
    ct4 = Util.toBytesFromString("4c");

    pt5 = Util.toBytesFromString("2e");
    ct5 = Util.toBytesFromString("9c");

    pt6 = Util.toBytesFromString("40");
    ct6 = Util.toBytesFromString("0d");

    pt7 = Util.toBytesFromString("9f");
    ct7 = Util.toBytesFromString("d4");

    pt8 = Util.toBytesFromString("96");
    ct8 = Util.toBytesFromString("36");

    pt9 = Util.toBytesFromString("e9");
    ct9 = Util.toBytesFromString("ba");

    pt10 = Util.toBytesFromString("3d");
    ct10 = Util.toBytesFromString("ce");

    pt11 = Util.toBytesFromString("7e");
    ct11 = Util.toBytesFromString("9e");

    pt12 = Util.toBytesFromString("11");
    ct12 = Util.toBytesFromString("0e");

    pt13 = Util.toBytesFromString("73");
    ct13 = Util.toBytesFromString("d4");

    pt14 = Util.toBytesFromString("93");
    ct14 = Util.toBytesFromString("58");

    pt15 = Util.toBytesFromString("17");
    ct15 = Util.toBytesFromString("6a");

    pt16 = Util.toBytesFromString("2a");
    ct16 = Util.toBytesFromString("4f");

    pt17 = Util.toBytesFromString("ae");
    ct17 = Util.toBytesFromString("32");

    pt18 = Util.toBytesFromString("2d");
    ct18 = Util.toBytesFromString("b9");

    pt = new byte[1];
    ct = new byte[1];
    mode = ModeFactory.getInstance(Registry.CFB_MODE, Registry.AES_CIPHER,
                                   128 / 8);
    attributes.clear();
    attributes.put(IMode.MODE_BLOCK_SIZE, new Integer(1));
    attributes.put(IMode.IV, iv);
    attributes.put(IMode.KEY_MATERIAL, key);
    try
      {
        // encryption ........................................................
        attributes.put(IMode.STATE, new Integer(IMode.ENCRYPTION));
        mode.init(attributes);

        mode.update(pt1, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct1), "CFB8-AES128-Encrypt block #1");

        mode.update(pt2, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct2), "CFB8-AES128-Encrypt block #2");

        mode.update(pt3, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct3), "CFB8-AES128-Encrypt block #3");

        mode.update(pt4, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct4), "CFB8-AES128-Encrypt block #4");

        mode.update(pt5, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct5), "CFB8-AES128-Encrypt block #5");

        mode.update(pt6, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct6), "CFB8-AES128-Encrypt block #6");

        mode.update(pt7, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct7), "CFB8-AES128-Encrypt block #7");

        mode.update(pt8, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct8), "CFB8-AES128-Encrypt block #8");

        mode.update(pt9, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct9), "CFB8-AES128-Encrypt block #9");

        mode.update(pt10, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct10), "CFB8-AES128-Encrypt block #10");

        mode.update(pt11, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct11), "CFB8-AES128-Encrypt block #11");

        mode.update(pt12, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct12), "CFB8-AES128-Encrypt block #12");

        mode.update(pt13, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct13), "CFB8-AES128-Encrypt block #13");

        mode.update(pt14, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct14), "CFB8-AES128-Encrypt block #14");

        mode.update(pt15, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct15), "CFB8-AES128-Encrypt block #15");

        mode.update(pt16, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct16), "CFB8-AES128-Encrypt block #16");

        mode.update(pt17, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct17), "CFB8-AES128-Encrypt block #17");

        mode.update(pt18, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct18), "CFB8-AES128-Encrypt block #18");

        // decryption ........................................................
        mode.reset();
        attributes.put(IMode.STATE, new Integer(IMode.DECRYPTION));
        mode.init(attributes);

        mode.update(ct1, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt1), "CFB8-AES128-Decrypt block #1");

        mode.update(ct2, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt2), "CFB8-AES128-Decrypt block #2");

        mode.update(ct3, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt3), "CFB8-AES128-Decrypt block #3");

        mode.update(ct4, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt4), "CFB8-AES128-Decrypt block #4");

        mode.update(ct5, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt5), "CFB8-AES128-Decrypt block #5");

        mode.update(ct6, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt6), "CFB8-AES128-Decrypt block #6");

        mode.update(ct7, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt7), "CFB8-AES128-Decrypt block #7");

        mode.update(ct8, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt8), "CFB8-AES128-Decrypt block #8");

        mode.update(ct9, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt9), "CFB8-AES128-Decrypt block #9");

        mode.update(ct10, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt10), "CFB8-AES128-Decrypt block #10");

        mode.update(ct11, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt11), "CFB8-AES128-Decrypt block #11");

        mode.update(ct12, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt12), "CFB8-AES128-Decrypt block #12");

        mode.update(ct13, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt13), "CFB8-AES128-Decrypt block #13");

        mode.update(ct14, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt14), "CFB8-AES128-Decrypt block #14");

        mode.update(ct15, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt15), "CFB8-AES128-Decrypt block #15");

        mode.update(ct16, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt16), "CFB8-AES128-Decrypt block #16");

        mode.update(ct17, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt17), "CFB8-AES128-Decrypt block #17");

        mode.update(ct18, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt18), "CFB8-AES128-Decrypt block #18");

      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfCFB8.testAES128");
      }

    harness.checkPoint("TestOfCFB8.testAES192");
    /** F.3.9 CFB8-AES192-Encrypt and F.3.10 CFB8-AES192-Decrypt. */
    key = Util.toBytesFromUnicode("\u8e73\ub0f7\uda0e\u6452\uc810\uf32b\u8090\u79e5"
                                  + "\u62f8\uead2\u522c\u6b7b");
    iv = Util.toBytesFromUnicode("\u0001\u0203\u0405\u0607\u0809\u0a0b\u0c0d\u0e0f");

    pt1 = Util.toBytesFromString("6b");
    ct1 = Util.toBytesFromString("cd");

    pt2 = Util.toBytesFromString("c1");
    ct2 = Util.toBytesFromString("a2");

    pt3 = Util.toBytesFromString("be");
    ct3 = Util.toBytesFromString("52");

    pt4 = Util.toBytesFromString("e2");
    ct4 = Util.toBytesFromString("1e");

    pt5 = Util.toBytesFromString("2e");
    ct5 = Util.toBytesFromString("f0");

    pt6 = Util.toBytesFromString("40");
    ct6 = Util.toBytesFromString("a9");

    pt7 = Util.toBytesFromString("9f");
    ct7 = Util.toBytesFromString("05");

    pt8 = Util.toBytesFromString("96");
    ct8 = Util.toBytesFromString("ca");

    pt9 = Util.toBytesFromString("e9");
    ct9 = Util.toBytesFromString("44");

    pt10 = Util.toBytesFromString("3d");
    ct10 = Util.toBytesFromString("cd");

    pt11 = Util.toBytesFromString("7e");
    ct11 = Util.toBytesFromString("05");

    pt12 = Util.toBytesFromString("11");
    ct12 = Util.toBytesFromString("7c");

    pt13 = Util.toBytesFromString("73");
    ct13 = Util.toBytesFromString("bf");

    pt14 = Util.toBytesFromString("93");
    ct14 = Util.toBytesFromString("0d");

    pt15 = Util.toBytesFromString("17");
    ct15 = Util.toBytesFromString("47");

    pt16 = Util.toBytesFromString("2a");
    ct16 = Util.toBytesFromString("a0");

    pt17 = Util.toBytesFromString("ae");
    ct17 = Util.toBytesFromString("67");

    pt18 = Util.toBytesFromString("2d");
    ct18 = Util.toBytesFromString("8a");

    ct = new byte[1];
    pt = new byte[1];

    mode = ModeFactory.getInstance(Registry.CFB_MODE, Registry.AES_CIPHER,
                                   128 / 8);
    attributes.clear();
    attributes.put(IMode.MODE_BLOCK_SIZE, new Integer(1));
    attributes.put(IMode.IV, iv);
    attributes.put(IMode.KEY_MATERIAL, key);
    try
      {
        // encryption ........................................................
        attributes.put(IMode.STATE, new Integer(IMode.ENCRYPTION));
        mode.init(attributes);

        mode.update(pt1, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct1), "CFB8-AES192-Encrypt block #1");

        mode.update(pt2, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct2), "CFB8-AES192-Encrypt block #2");

        mode.update(pt3, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct3), "CFB8-AES192-Encrypt block #3");

        mode.update(pt4, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct4), "CFB8-AES192-Encrypt block #4");

        mode.update(pt5, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct5), "CFB8-AES192-Encrypt block #5");

        mode.update(pt6, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct6), "CFB8-AES192-Encrypt block #6");

        mode.update(pt7, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct7), "CFB8-AES192-Encrypt block #7");

        mode.update(pt8, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct8), "CFB8-AES192-Encrypt block #8");

        mode.update(pt9, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct9), "CFB8-AES192-Encrypt block #9");

        mode.update(pt10, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct10), "CFB8-AES192-Encrypt block #10");

        mode.update(pt11, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct11), "CFB8-AES192-Encrypt block #11");

        mode.update(pt12, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct12), "CFB8-AES192-Encrypt block #12");

        mode.update(pt13, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct13), "CFB8-AES192-Encrypt block #13");

        mode.update(pt14, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct14), "CFB8-AES192-Encrypt block #14");

        mode.update(pt15, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct15), "CFB8-AES192-Encrypt block #15");

        mode.update(pt16, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct16), "CFB8-AES192-Encrypt block #16");

        mode.update(pt17, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct17), "CFB8-AES192-Encrypt block #17");

        mode.update(pt18, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct18), "CFB8-AES192-Encrypt block #18");

        // decryption ........................................................
        mode.reset();
        attributes.put(IMode.STATE, new Integer(IMode.DECRYPTION));
        mode.init(attributes);

        mode.update(ct1, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt1), "CFB8-AES192-Decrypt block #1");

        mode.update(ct2, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt2), "CFB8-AES192-Decrypt block #2");

        mode.update(ct3, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt3), "CFB8-AES192-Decrypt block #3");

        mode.update(ct4, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt4), "CFB8-AES192-Decrypt block #4");

        mode.update(ct5, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt5), "CFB8-AES192-Decrypt block #5");

        mode.update(ct6, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt6), "CFB8-AES192-Decrypt block #6");

        mode.update(ct7, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt7), "CFB8-AES192-Decrypt block #7");

        mode.update(ct8, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt8), "CFB8-AES192-Decrypt block #8");

        mode.update(ct9, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt9), "CFB8-AES192-Decrypt block #9");

        mode.update(ct10, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt10), "CFB8-AES192-Decrypt block #10");

        mode.update(ct11, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt11), "CFB8-AES192-Decrypt block #11");

        mode.update(ct12, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt12), "CFB8-AES192-Decrypt block #12");

        mode.update(ct13, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt13), "CFB8-AES192-Decrypt block #13");

        mode.update(ct14, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt14), "CFB8-AES192-Decrypt block #14");

        mode.update(ct15, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt15), "CFB8-AES192-Decrypt block #15");

        mode.update(ct16, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt16), "CFB8-AES192-Decrypt block #16");

        mode.update(ct17, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt17), "CFB8-AES192-Decrypt block #17");

        mode.update(ct18, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt18), "CFB8-AES192-Decrypt block #18");

      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfCFB8.testAES192");
      }

    harness.checkPoint("TestOfCFB8.testAES256");
    /** F.3.11 CFB8-AES256-Encrypt and F.3.12 CFB8-AES256-Decrypt. */
    key = Util.toBytesFromUnicode("\u603d\ueb10\u15ca\u71be\u2b73\uaef0\u857d\u7781"
                                  + "\u1f35\u2c07\u3b61\u08d7\u2d98\u10a3\u0914\udff4");
    iv = Util.toBytesFromUnicode("\u0001\u0203\u0405\u0607\u0809\u0a0b\u0c0d\u0e0f");

    pt1 = Util.toBytesFromString("6b");
    ct1 = Util.toBytesFromString("dc");

    pt2 = Util.toBytesFromString("c1");
    ct2 = Util.toBytesFromString("1f");

    pt3 = Util.toBytesFromString("be");
    ct3 = Util.toBytesFromString("1a");

    pt4 = Util.toBytesFromString("e2");
    ct4 = Util.toBytesFromString("85");

    pt5 = Util.toBytesFromString("2e");
    ct5 = Util.toBytesFromString("20");

    pt6 = Util.toBytesFromString("40");
    ct6 = Util.toBytesFromString("a6");

    pt7 = Util.toBytesFromString("9f");
    ct7 = Util.toBytesFromString("4d");

    pt8 = Util.toBytesFromString("96");
    ct8 = Util.toBytesFromString("b5");

    pt9 = Util.toBytesFromString("e9");
    ct9 = Util.toBytesFromString("5f");

    pt10 = Util.toBytesFromString("3d");
    ct10 = Util.toBytesFromString("cc");

    pt11 = Util.toBytesFromString("7e");
    ct11 = Util.toBytesFromString("8a");

    pt12 = Util.toBytesFromString("11");
    ct12 = Util.toBytesFromString("c5");

    pt13 = Util.toBytesFromString("73");
    ct13 = Util.toBytesFromString("54");

    pt14 = Util.toBytesFromString("93");
    ct14 = Util.toBytesFromString("84");

    pt15 = Util.toBytesFromString("17");
    ct15 = Util.toBytesFromString("4e");

    pt16 = Util.toBytesFromString("2a");
    ct16 = Util.toBytesFromString("88");

    pt17 = Util.toBytesFromString("ae");
    ct17 = Util.toBytesFromString("97");

    pt18 = Util.toBytesFromString("2d");
    ct18 = Util.toBytesFromString("00");

    ct = new byte[1];
    pt = new byte[1];

    mode = ModeFactory.getInstance(Registry.CFB_MODE, Registry.AES_CIPHER,
                                   128 / 8);
    attributes.clear();
    attributes.put(IMode.MODE_BLOCK_SIZE, new Integer(1));
    attributes.put(IMode.IV, iv);
    attributes.put(IMode.KEY_MATERIAL, key);
    try
      {
        // encryption ........................................................
        attributes.put(IMode.STATE, new Integer(IMode.ENCRYPTION));
        mode.init(attributes);

        mode.update(pt1, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct1), "CFB8-AES256-Encrypt block #1");

        mode.update(pt2, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct2), "CFB8-AES256-Encrypt block #2");

        mode.update(pt3, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct3), "CFB8-AES256-Encrypt block #3");

        mode.update(pt4, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct4), "CFB8-AES256-Encrypt block #4");

        mode.update(pt5, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct5), "CFB8-AES256-Encrypt block #5");

        mode.update(pt6, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct6), "CFB8-AES256-Encrypt block #6");

        mode.update(pt7, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct7), "CFB8-AES256-Encrypt block #7");

        mode.update(pt8, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct8), "CFB8-AES256-Encrypt block #8");

        mode.update(pt9, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct9), "CFB8-AES256-Encrypt block #9");

        mode.update(pt10, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct10), "CFB8-AES256-Encrypt block #10");

        mode.update(pt11, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct11), "CFB8-AES256-Encrypt block #11");

        mode.update(pt12, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct12), "CFB8-AES256-Encrypt block #12");

        mode.update(pt13, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct13), "CFB8-AES256-Encrypt block #13");

        mode.update(pt14, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct14), "CFB8-AES256-Encrypt block #14");

        mode.update(pt15, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct15), "CFB8-AES256-Encrypt block #15");

        mode.update(pt16, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct16), "CFB8-AES256-Encrypt block #16");

        mode.update(pt17, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct17), "CFB8-AES256-Encrypt block #17");

        mode.update(pt18, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct18), "CFB8-AES256-Encrypt block #18");

        // decryption ........................................................
        mode.reset();
        attributes.put(IMode.STATE, new Integer(IMode.DECRYPTION));
        mode.init(attributes);

        mode.update(ct1, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt1), "CFB8-AES256-Decrypt block #1");

        mode.update(ct2, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt2), "CFB8-AES256-Decrypt block #2");

        mode.update(ct3, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt3), "CFB8-AES256-Decrypt block #3");

        mode.update(ct4, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt4), "CFB8-AES256-Decrypt block #4");

        mode.update(ct5, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt5), "CFB8-AES256-Decrypt block #5");

        mode.update(ct6, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt6), "CFB8-AES256-Decrypt block #6");

        mode.update(ct7, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt7), "CFB8-AES256-Decrypt block #7");

        mode.update(ct8, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt8), "CFB8-AES256-Decrypt block #8");

        mode.update(ct9, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt9), "CFB8-AES256-Decrypt block #9");

        mode.update(ct10, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt10), "CFB8-AES256-Decrypt block #10");

        mode.update(ct11, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt11), "CFB8-AES256-Decrypt block #11");

        mode.update(ct12, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt12), "CFB8-AES256-Decrypt block #12");

        mode.update(ct13, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt13), "CFB8-AES256-Decrypt block #13");

        mode.update(ct14, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt14), "CFB8-AES256-Decrypt block #14");

        mode.update(ct15, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt15), "CFB8-AES256-Decrypt block #15");

        mode.update(ct16, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt16), "CFB8-AES256-Decrypt block #16");

        mode.update(ct17, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt17), "CFB8-AES256-Decrypt block #17");

        mode.update(ct18, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt18), "CFB8-AES256-Decrypt block #18");

      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfCFB8.testAES256");
      }

    harness.checkPoint("TestOfCFB128.testAES128");
    /** F.3.13 CFB128-AES128-Encrypt and F.3.14 CFB128-AES128-Decrypt. */
    key = Util.toBytesFromString("2b7e151628aed2a6abf7158809cf4f3c");
    iv = Util.toBytesFromString("000102030405060708090a0b0c0d0e0f");

    pt1 = Util.toBytesFromString("6bc1bee22e409f96e93d7e117393172a");
    ct1 = Util.toBytesFromString("3b3fd92eb72dad20333449f8e83cfb4a");

    pt2 = Util.toBytesFromString("ae2d8a571e03ac9c9eb76fac45af8e51");
    ct2 = Util.toBytesFromString("c8a64537a0b3a93fcde3cdad9f1ce58b");

    pt3 = Util.toBytesFromString("30c81c46a35ce411e5fbc1191a0a52ef");
    ct3 = Util.toBytesFromString("26751f67a3cbb140b1808cf187a4f4df");

    pt4 = Util.toBytesFromString("f69f2445df4f9b17ad2b417be66c3710");
    ct4 = Util.toBytesFromString("c04b05357c5d1c0eeac4c66f9ff7f2e6");

    pt = new byte[16];
    ct = new byte[16];

    mode = ModeFactory.getInstance(Registry.CFB_MODE, Registry.AES_CIPHER,
                                   128 / 8);
    attributes.clear();
    attributes.put(IMode.MODE_BLOCK_SIZE, new Integer(16));
    attributes.put(IMode.IV, iv);
    attributes.put(IMode.KEY_MATERIAL, key);
    try
      {
        // encryption ......................................................
        attributes.put(IMode.STATE, new Integer(IMode.ENCRYPTION));
        mode.init(attributes);

        mode.update(pt1, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct1), "CFB128-AES128-Encrypt block #1");

        mode.update(pt2, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct2), "CFB128-AES128-Encrypt block #2");

        mode.update(pt3, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct3), "CFB128-AES128-Encrypt block #3");

        mode.update(pt4, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct4), "CFB128-AES128-Encrypt block #4");

        // decryption ......................................................
        mode.reset();
        attributes.put(IMode.STATE, new Integer(IMode.DECRYPTION));
        mode.init(attributes);

        mode.update(ct1, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt1), "CFB128-AES128-Decrypt block #1");

        mode.update(ct2, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt2), "CFB128-AES128-Decrypt block #2");

        mode.update(ct3, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt3), "CFB128-AES128-Decrypt block #3");

        mode.update(ct4, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt4), "CFB128-AES128-Decrypt block #4");

      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfCFB128.testAES128");
      }

    harness.checkPoint("TestOfCFB128.testAES192");
    /** F.3.15 CFB128-AES192-Encrypt and F.3.16 CFB128-AES192-Decrypt. */
    key = Util.toBytesFromString("8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b");
    iv = Util.toBytesFromString("000102030405060708090a0b0c0d0e0f");

    pt1 = Util.toBytesFromString("6bc1bee22e409f96e93d7e117393172a");
    ct1 = Util.toBytesFromString("cdc80d6fddf18cab34c25909c99a4174");

    pt2 = Util.toBytesFromString("ae2d8a571e03ac9c9eb76fac45af8e51");
    ct2 = Util.toBytesFromString("67ce7f7f81173621961a2b70171d3d7a");

    pt3 = Util.toBytesFromString("30c81c46a35ce411e5fbc1191a0a52ef");
    ct3 = Util.toBytesFromString("2e1e8a1dd59b88b1c8e60fed1efac4c9");

    pt4 = Util.toBytesFromString("f69f2445df4f9b17ad2b417be66c3710");
    ct4 = Util.toBytesFromString("c05f9f9ca9834fa042ae8fba584b09ff");

    pt = new byte[16];
    ct = new byte[16];

    mode = ModeFactory.getInstance(Registry.CFB_MODE, Registry.AES_CIPHER,
                                   128 / 8);
    attributes.clear();
    attributes.put(IMode.MODE_BLOCK_SIZE, new Integer(16));
    attributes.put(IMode.IV, iv);
    attributes.put(IMode.KEY_MATERIAL, key);
    try
      {
        // encryption ......................................................
        attributes.put(IMode.STATE, new Integer(IMode.ENCRYPTION));
        mode.init(attributes);

        mode.update(pt1, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct1), "CFB128-AES192-Encrypt block #1");

        mode.update(pt2, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct2), "CFB128-AES192-Encrypt block #2");

        mode.update(pt3, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct3), "CFB128-AES192-Encrypt block #3");

        mode.update(pt4, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct4), "CFB128-AES192-Encrypt block #4");

        // decryption ......................................................
        mode.reset();
        attributes.put(IMode.STATE, new Integer(IMode.DECRYPTION));
        mode.init(attributes);

        mode.update(ct1, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt1), "CFB128-AES192-Decrypt block #1");

        mode.update(ct2, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt2), "CFB128-AES192-Decrypt block #2");

        mode.update(ct3, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt3), "CFB128-AES192-Decrypt block #3");

        mode.update(ct4, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt4), "CFB128-AES192-Decrypt block #4");

      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfCFB128.testAES192");
      }

    harness.checkPoint("TestOfCFB128.testAES256");
    /** F.3.17 CFB128-AES256-Encrypt and F.3.18 CFB128-AES256-Decrypt. */
    key = Util.toBytesFromString("603deb1015ca71be2b73aef0857d7781"
                                 + "1f352c073b6108d72d9810a30914dff4");
    iv = Util.toBytesFromString("000102030405060708090a0b0c0d0e0f");

    pt1 = Util.toBytesFromString("6bc1bee22e409f96e93d7e117393172a");
    ct1 = Util.toBytesFromString("dc7e84bfda79164b7ecd8486985d3860");

    pt2 = Util.toBytesFromString("ae2d8a571e03ac9c9eb76fac45af8e51");
    ct2 = Util.toBytesFromString("39ffed143b28b1c832113c6331e5407b");

    pt3 = Util.toBytesFromString("30c81c46a35ce411e5fbc1191a0a52ef");
    ct3 = Util.toBytesFromString("df10132415e54b92a13ed0a8267ae2f9");

    pt4 = Util.toBytesFromString("f69f2445df4f9b17ad2b417be66c3710");
    ct4 = Util.toBytesFromString("75a385741ab9cef82031623d55b1e471");

    pt = new byte[16];
    ct = new byte[16];

    mode = ModeFactory.getInstance(Registry.CFB_MODE, Registry.AES_CIPHER,
                                   128 / 8);
    attributes.clear();
    attributes.put(IMode.MODE_BLOCK_SIZE, new Integer(16));
    attributes.put(IMode.IV, iv);
    attributes.put(IMode.KEY_MATERIAL, key);
    try
      {
        // encryption ......................................................
        attributes.put(IMode.STATE, new Integer(IMode.ENCRYPTION));
        mode.init(attributes);

        mode.update(pt1, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct1), "CFB128-AES256-Encrypt block #1");

        mode.update(pt2, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct2), "CFB128-AES256-Encrypt block #2");

        mode.update(pt3, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct3), "CFB128-AES256-Encrypt block #3");

        mode.update(pt4, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct4), "CFB128-AES256-Encrypt block #4");

        // decryption ......................................................
        mode.reset();
        attributes.put(IMode.STATE, new Integer(IMode.DECRYPTION));
        mode.init(attributes);

        mode.update(ct1, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt1), "CFB128-AES256-Decrypt block #1");

        mode.update(ct2, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt2), "CFB128-AES256-Decrypt block #2");

        mode.update(ct3, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt3), "CFB128-AES256-Decrypt block #3");

        mode.update(ct4, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt4), "CFB128-AES256-Decrypt block #4");

      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfCFB128.testAES256");
      }
  }
}