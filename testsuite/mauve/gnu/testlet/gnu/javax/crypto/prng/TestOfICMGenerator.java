/* TestOfICMGenerator.java -- 
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

package gnu.testlet.gnu.javax.crypto.prng;

import gnu.java.security.prng.IRandom;
import gnu.java.security.util.Util;
import gnu.javax.crypto.prng.ICMGenerator;
import gnu.javax.crypto.prng.PRNGFactory;
import gnu.javax.crypto.cipher.IBlockCipher;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import java.math.BigInteger;
import java.util.HashMap;

/**
 * Conformance test for the ICM implementation. Tests if the output matches
 * the values given in the draft-mcgrew-saag-icm-00.txt.
 */
public class TestOfICMGenerator implements Testlet
{
  private HashMap map = new HashMap();

  private byte[] key, offset, data;

  private ICMGenerator icm = new ICMGenerator();

  private IRandom rnd = PRNGFactory.getInstance("icm");

  private String ks, computed;

  public void test(TestHarness harness)
  {
    harness.checkPoint("TestOfICMGenerator.testVectorOne");
    // Block Cipher Key:     000102030405060708090A0B0C0D0E0F
    // Offset:               000102030405060708090A0B0C0D0E0F
    // BLOCK_INDEX_LENGTH:   4
    // SEGMENT_INDEX_LENGTH: 4
    // Segment Index:        00000000
    //
    // Counter                          Keystream
    // 000102030405060708090A0B0C0D0E0F 0A940BB5416EF045F1C39458C653EA5A
    // 000102030405060708090A0B0C0D0E10 0263EC94661872969ADAFD0F4BA40FDC
    // 000102030405060708090A0B0C0D0E11 1A2D94B3111CA5F8BDC2C84DCC29EC47
    // 000102030405060708090A0B0C0D0E12 4D0BABD2995F9F076223246847B5D30E
    // 000102030405060708090A0B0C0D0E13 8D33F128463B88EFD3F8A52505020379
    key = new byte[16];
    offset = new byte[16];
    for (int i = 0; i < 16; i++)
      {
        key[i] = (byte) i;
        offset[i] = (byte) i;
      }

    map.clear();
    map.put(IBlockCipher.CIPHER_BLOCK_SIZE, new Integer(16));
    map.put(IBlockCipher.KEY_MATERIAL, key);
    map.put(ICMGenerator.SEGMENT_INDEX_LENGTH, new Integer(4));
    map.put(ICMGenerator.OFFSET, offset);
    map.put(ICMGenerator.SEGMENT_INDEX, BigInteger.ZERO);

    data = new byte[16];
    try
      {
        icm.init(map);

        ks = "0A940BB5416EF045F1C39458C653EA5A";
        icm.nextBytes(data, 0, 16);
        computed = Util.toString(data);
        harness.check(ks.equals(computed));

        ks = "0263EC94661872969ADAFD0F4BA40FDC";
        icm.nextBytes(data, 0, 16);
        computed = Util.toString(data);
        harness.check(ks.equals(computed));

        ks = "1A2D94B3111CA5F8BDC2C84DCC29EC47";
        icm.nextBytes(data, 0, 16);
        computed = Util.toString(data);
        harness.check(ks.equals(computed));

        ks = "4D0BABD2995F9F076223246847B5D30E";
        icm.nextBytes(data, 0, 16);
        computed = Util.toString(data);
        harness.check(ks.equals(computed));

        ks = "8D33F128463B88EFD3F8A52505020379";
        icm.nextBytes(data, 0, 16);
        computed = Util.toString(data);
        harness.check(ks.equals(computed));
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfICMGenerator.testVectorOne");
      }

    harness.checkPoint("TestOfICMGenerator.testVectorTwo");
    // Block Cipher Key:     75387824D1F1F3815641B65D78D51EDB
    // Offset:               96C9781981053CBBCB36927844F1932C
    // BLOCK_INDEX_LENGTH:   2
    // SEGMENT_INDEX_LENGTH: 6
    // Segment Index:        12345678
    //
    // Counter                          Keystream
    // 96C9781981053CBBCB36A4AC9B69932C EA0AA027BA6D56E44B28F43A7E3E5F58
    // 96C9781981053CBBCB36A4AC9B69932D CBDB3107EDA8D420D3EF7AB7FF290166
    // 96C9781981053CBBCB36A4AC9B69932E AED6F7CB14ED49174336CC010AEB8780
    // 96C9781981053CBBCB36A4AC9B69932F 4C3A754AF027A5C8CCB40E0FE20AF246
    // 96C9781981053CBBCB36A4AC9B699330 01A6D1CE983EF993E980CC9568587E3D
    key = new byte[] { (byte) 0x75, (byte) 0x38, (byte) 0x78, (byte) 0x24,
                      (byte) 0xD1, (byte) 0xF1, (byte) 0xF3, (byte) 0x81,
                      (byte) 0x56, (byte) 0x41, (byte) 0xB6, (byte) 0x5D,
                      (byte) 0x78, (byte) 0xD5, (byte) 0x1E, (byte) 0xDB };

    offset = new byte[] { (byte) 0x96, (byte) 0xC9, (byte) 0x78, (byte) 0x19,
                         (byte) 0x81, (byte) 0x05, (byte) 0x3C, (byte) 0xBB,
                         (byte) 0xCB, (byte) 0x36, (byte) 0x92, (byte) 0x78,
                         (byte) 0x44, (byte) 0xF1, (byte) 0x93, (byte) 0x2C };

    map.clear();
    map.put(IBlockCipher.CIPHER_BLOCK_SIZE, new Integer(16));
    map.put(IBlockCipher.KEY_MATERIAL, key);
    map.put(ICMGenerator.BLOCK_INDEX_LENGTH, new Integer(2));
    map.put(ICMGenerator.OFFSET, new BigInteger(1, offset));
    map.put(ICMGenerator.SEGMENT_INDEX, new BigInteger("12345678", 16));

    data = new byte[16];
    String ks, computed;
    try
      {
        rnd.init(map);

        ks = "EA0AA027BA6D56E44B28F43A7E3E5F58";
        rnd.nextBytes(data, 0, 16);
        computed = Util.toString(data);
        harness.check(ks.equals(computed));

        ks = "CBDB3107EDA8D420D3EF7AB7FF290166";
        rnd.nextBytes(data, 0, 16);
        computed = Util.toString(data);
        harness.check(ks.equals(computed));

        ks = "AED6F7CB14ED49174336CC010AEB8780";
        rnd.nextBytes(data, 0, 16);
        computed = Util.toString(data);
        harness.check(ks.equals(computed));

        ks = "4C3A754AF027A5C8CCB40E0FE20AF246";
        rnd.nextBytes(data, 0, 16);
        computed = Util.toString(data);
        harness.check(ks.equals(computed));

        ks = "01A6D1CE983EF993E980CC9568587E3D";
        rnd.nextBytes(data, 0, 16);
        computed = Util.toString(data);
        harness.check(ks.equals(computed));

      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfICMGenerator.testVectorTwo");
      }
  }
}