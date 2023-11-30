/* TestOfOFB.java -- 
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
 * Conformance tests of the OFB implementation.
 *
 * <p>References:</p>
 *
 * <ol>
 *    <li><a href="http://csrc.nist.gov/encryption/modes/Recommendation/Modes01.pdf">
 *    Recommendation for Block Cipher Modes of Operation Methods and Techniques</a>,
 *    Morris Dworkin.</li>
 * </ol>
 */
public class TestOfOFB implements Testlet
{
  private byte[] key, iv, pt1, ct1, pt2, ct2, pt3, ct3, pt4, ct4, pt, ct;

  private IMode mode;

  private Map attributes = new HashMap();

  public void test(TestHarness harness)
  {
    harness.checkPoint("TestOfOFB.testAES128");
    /** F.4.1 OFB-AES128-Encrypt and F.4.2 OFB-AES128-Decrypt. */
    key = Util.toBytesFromUnicode("\u2b7e\u1516\u28ae\ud2a6\uabf7\u1588\u09cf\u4f3c");
    iv = Util.toBytesFromUnicode("\u0001\u0203\u0405\u0607\u0809\u0a0b\u0c0d\u0e0f");

    pt1 = Util.toBytesFromUnicode("\u6bc1\ubee2\u2e40\u9f96\ue93d\u7e11\u7393\u172a");
    ct1 = Util.toBytesFromUnicode("\u3b3f\ud92e\ub72d\uad20\u3334\u49f8\ue83c\ufb4a");

    pt2 = Util.toBytesFromUnicode("\uae2d\u8a57\u1e03\uac9c\u9eb7\u6fac\u45af\u8e51");
    ct2 = Util.toBytesFromUnicode("\u7789\u508d\u1691\u8f03\uf53c\u52da\uc54e\ud825");

    pt3 = Util.toBytesFromUnicode("\u30c8\u1c46\ua35c\ue411\ue5fb\uc119\u1a0a\u52ef");
    ct3 = Util.toBytesFromUnicode("\u9740\u051e\u9c5f\uecf6\u4344\uf7a8\u2260\uedcc");

    pt4 = Util.toBytesFromUnicode("\uf69f\u2445\udf4f\u9b17\uad2b\u417b\ue66c\u3710");
    ct4 = Util.toBytesFromUnicode("\u304c\u6528\uf659\uc778\u66a5\u10d9\uc1d6\uae5e");

    ct = new byte[16];
    pt = new byte[16];

    mode = ModeFactory.getInstance(Registry.OFB_MODE, Registry.AES_CIPHER,
                                   128 / 8);
    attributes.clear();
    attributes.put(IMode.IV, iv);
    attributes.put(IMode.KEY_MATERIAL, key);
    try
      {
        // encryption ........................................................
        attributes.put(IMode.STATE, new Integer(IMode.ENCRYPTION));
        mode.init(attributes);

        mode.update(pt1, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct1), "OFB-AES128-Decrypt block #1");

        mode.update(pt2, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct2), "OFB-AES128-Decrypt block #2");

        mode.update(pt3, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct3), "OFB-AES128-Decrypt block #3");

        mode.update(pt4, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct4), "OFB-AES128-Decrypt block #4");

        // decryption ........................................................
        mode.reset();
        attributes.put(IMode.STATE, new Integer(IMode.DECRYPTION));
        mode.init(attributes);

        mode.update(ct1, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt1), "OFB-AES128-Decrypt block #1");

        mode.update(ct2, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt2), "OFB-AES128-Decrypt block #2");

        mode.update(ct3, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt3), "OFB-AES128-Decrypt block #3");

        mode.update(ct4, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt4), "OFB-AES128-Decrypt block #4");

      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfOFB.testAES128");
      }

    harness.checkPoint("TestOfOFB.testAES192");
    /** F.4.3 OFB-AES192-Encrypt and F.4.4 OFB-AES192-Decrypt. */
    key = Util.toBytesFromUnicode("\u8e73\ub0f7\uda0e\u6452\uc810\uf32b\u8090\u79e5"
                                  + "\u62f8\uead2\u522c\u6b7b");
    iv = Util.toBytesFromUnicode("\u0001\u0203\u0405\u0607\u0809\u0a0b\u0c0d\u0e0f");

    pt1 = Util.toBytesFromUnicode("\u6bc1\ubee2\u2e40\u9f96\ue93d\u7e11\u7393\u172a");
    ct1 = Util.toBytesFromUnicode("\ucdc8\u0d6f\uddf1\u8cab\u34c2\u5909\uc99a\u4174");

    pt2 = Util.toBytesFromUnicode("\uae2d\u8a57\u1e03\uac9c\u9eb7\u6fac\u45af\u8e51");
    ct2 = Util.toBytesFromUnicode("\ufcc2\u8b8d\u4c63\u837c\u09e8\u1700\uc110\u0401");

    pt3 = Util.toBytesFromUnicode("\u30c8\u1c46\ua35c\ue411\ue5fb\uc119\u1a0a\u52ef");
    ct3 = Util.toBytesFromUnicode("\u8d9a\u9aea\uc0f6\u596f\u559c\u6d4d\uaf59\ua5f2");

    pt4 = Util.toBytesFromUnicode("\uf69f\u2445\udf4f\u9b17\uad2b\u417b\ue66c\u3710");
    ct4 = Util.toBytesFromUnicode("\u6d9f\u2008\u57ca\u6c3e\u9cac\u524b\ud9ac\uc92a");

    ct = new byte[16];
    pt = new byte[16];

    mode = ModeFactory.getInstance(Registry.OFB_MODE, Registry.AES_CIPHER,
                                   128 / 8);
    attributes.clear();
    attributes.put(IMode.IV, iv);
    attributes.put(IMode.KEY_MATERIAL, key);
    try
      {
        // encryption ........................................................
        attributes.put(IMode.STATE, new Integer(IMode.ENCRYPTION));
        mode.init(attributes);

        mode.update(pt1, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct1), "OFB-AES192-Decrypt block #1");

        mode.update(pt2, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct2), "OFB-AES192-Decrypt block #2");

        mode.update(pt3, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct3), "OFB-AES192-Decrypt block #3");

        mode.update(pt4, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct4), "OFB-AES192-Decrypt block #4");

        // decryption ........................................................
        mode.reset();
        attributes.put(IMode.STATE, new Integer(IMode.DECRYPTION));
        mode.init(attributes);

        mode.update(ct1, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt1), "OFB-AES192-Decrypt block #1");

        mode.update(ct2, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt2), "OFB-AES192-Decrypt block #2");

        mode.update(ct3, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt3), "OFB-AES192-Decrypt block #3");

        mode.update(ct4, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt4), "OFB-AES192-Decrypt block #4");

      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfOFB.testAES192");
      }

    harness.checkPoint("TestOfOFB.testAES256");
    /** F.4.5 OFB-AES256-Encrypt and F.4.6 OFB-AES256-Decrypt. */
    key = Util.toBytesFromUnicode("\u603d\ueb10\u15ca\u71be\u2b73\uaef0\u857d\u7781"
                                  + "\u1f35\u2c07\u3b61\u08d7\u2d98\u10a3\u0914\udff4");

    iv = Util.toBytesFromUnicode("\u0001\u0203\u0405\u0607\u0809\u0a0b\u0c0d\u0e0f");

    pt1 = Util.toBytesFromUnicode("\u6bc1\ubee2\u2e40\u9f96\ue93d\u7e11\u7393\u172a");
    ct1 = Util.toBytesFromUnicode("\udc7e\u84bf\uda79\u164b\u7ecd\u8486\u985d\u3860");

    pt2 = Util.toBytesFromUnicode("\uae2d\u8a57\u1e03\uac9c\u9eb7\u6fac\u45af\u8e51");
    ct2 = Util.toBytesFromUnicode("\u4feb\udc67\u40d2\u0b3a\uc88f\u6ad8\u2a4f\ub08d");

    pt3 = Util.toBytesFromUnicode("\u30c8\u1c46\ua35c\ue411\ue5fb\uc119\u1a0a\u52ef");
    ct3 = Util.toBytesFromUnicode("\u71ab\u47a0\u86e8\u6eed\uf39d\u1c5b\uba97\uc408");

    pt4 = Util.toBytesFromUnicode("\uf69f\u2445\udf4f\u9b17\uad2b\u417b\ue66c\u3710");
    ct4 = Util.toBytesFromUnicode("\u0126\u141d\u67f3\u7be8\u538f\u5a8b\ue740\ue484");

    ct = new byte[16];
    pt = new byte[16];

    mode = ModeFactory.getInstance(Registry.OFB_MODE, Registry.AES_CIPHER,
                                   128 / 8);
    attributes.clear();
    attributes.put(IMode.IV, iv);
    attributes.put(IMode.KEY_MATERIAL, key);
    try
      {
        // encryption ........................................................
        attributes.put(IMode.STATE, new Integer(IMode.ENCRYPTION));
        mode.init(attributes);

        mode.update(pt1, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct1), "OFB-AES256-Decrypt block #1");

        mode.update(pt2, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct2), "OFB-AES256-Decrypt block #2");

        mode.update(pt3, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct3), "OFB-AES256-Decrypt block #3");

        mode.update(pt4, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct4), "OFB-AES256-Decrypt block #4");

        // decryption ........................................................
        mode.reset();
        attributes.put(IMode.STATE, new Integer(IMode.DECRYPTION));
        mode.init(attributes);

        mode.update(ct1, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt1), "OFB-AES256-Decrypt block #1");

        mode.update(ct2, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt2), "OFB-AES256-Decrypt block #2");

        mode.update(ct3, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt3), "OFB-AES256-Decrypt block #3");

        mode.update(ct4, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt4), "OFB-AES256-Decrypt block #4");

      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfOFB.testAES256");
      }
  }
}