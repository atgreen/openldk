/* TestOfCBC.java -- 
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
 * Conformance tests of the CBC implementation.
 *
 * <p>References:</p>
 * <ol>
 *    <li><a href="http://csrc.nist.gov/encryption/modes/Recommendation/Modes01.pdf">
 *    Recommendation for Block Cipher Modes of Operation Methods and Techniques</a>,
 *    Morris Dworkin.</li>
 * </ol>
 */
public class TestOfCBC implements Testlet
{
  private byte[] key, iv, pt1, ct1, pt2, ct2, pt3, ct3, pt4, ct4, pt, ct;

  private IMode mode;

  private Map attributes = new HashMap();

  public void test(TestHarness harness)
  {
    harness.checkPoint("TestOfCBC.testAES128");
    /** F.2.1 CBC-AES-Encrypt and F.2.2 CBC-AES-Decrypt. */
    key = Util.toBytesFromUnicode("\u2b7e\u1516\u28ae\ud2a6\uabf7\u1588\u09cf\u4f3c");
    iv = Util.toBytesFromUnicode("\u0001\u0203\u0405\u0607\u0809\u0a0b\u0c0d\u0e0f");

    pt1 = Util.toBytesFromUnicode("\u6bc1\ubee2\u2e40\u9f96\ue93d\u7e11\u7393\u172a");
    ct1 = Util.toBytesFromUnicode("\u7649\uabac\u8119\ub246\ucee9\u8e9b\u12e9\u197d");

    pt2 = Util.toBytesFromUnicode("\uae2d\u8a57\u1e03\uac9c\u9eb7\u6fac\u45af\u8e51");
    ct2 = Util.toBytesFromUnicode("\u5086\ucb9b\u5072\u19ee\u95db\u113a\u9176\u78b2");

    pt3 = Util.toBytesFromUnicode("\u30c8\u1c46\ua35c\ue411\ue5fb\uc119\u1a0a\u52ef");
    ct3 = Util.toBytesFromUnicode("\u73be\ud6b8\ue3c1\u743b\u7116\ue69e\u2222\u9516");

    pt4 = Util.toBytesFromUnicode("\uf69f\u2445\udf4f\u9b17\uad2b\u417b\ue66c\u3710");
    ct4 = Util.toBytesFromUnicode("\u3ff1\ucaa1\u681f\uac09\u120e\uca30\u7586\ue1a7");

    pt = new byte[16];
    ct = new byte[16];
    mode = ModeFactory.getInstance(Registry.CBC_MODE, Registry.AES_CIPHER,
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
        harness.check(Arrays.equals(ct, ct1), "CBC-AES128-Encrypt block #1");

        mode.update(pt2, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct2), "CBC-AES128-Encrypt block #2");

        mode.update(pt3, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct3), "CBC-AES128-Encrypt block #3");

        mode.update(pt4, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct4), "CBC-AES128-Encrypt block #4");

        // decryption ........................................................
        mode.reset();
        attributes.put(IMode.STATE, new Integer(IMode.DECRYPTION));
        mode.init(attributes);

        mode.update(ct1, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt1), "CBC-AES128-Decrypt block #1");

        mode.update(ct2, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt2), "CBC-AES128-Decrypt block #2");

        mode.update(ct3, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt3), "CBC-AES128-Decrypt block #3");

        mode.update(ct4, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt4), "CBC-AES128-Decrypt block #4");

      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfCBC.testAES128");
      }

    harness.checkPoint("TestOfCBC.testAES192");
    /** F.2.3 CBC-AES192-Encrypt and F.2.4 CBC-AES192-Decrypt. */
    key = Util.toBytesFromUnicode("\u8e73\ub0f7\uda0e\u6452\uc810\uf32b\u8090\u79e5"
                                  + "\u62f8\uead2\u522c\u6b7b");
    iv = Util.toBytesFromUnicode("\u0001\u0203\u0405\u0607\u0809\u0a0b\u0c0d\u0e0f");

    pt1 = Util.toBytesFromUnicode("\u6bc1\ubee2\u2e40\u9f96\ue93d\u7e11\u7393\u172a");
    ct1 = Util.toBytesFromUnicode("\u4f02\u1db2\u43bc\u633d\u7178\u183a\u9fa0\u71e8");

    pt2 = Util.toBytesFromUnicode("\uae2d\u8a57\u1e03\uac9c\u9eb7\u6fac\u45af\u8e51");
    ct2 = Util.toBytesFromUnicode("\ub4d9\uada9\uad7d\uedf4\ue5e7\u3876\u3f69\u145a");

    pt3 = Util.toBytesFromUnicode("\u30c8\u1c46\ua35c\ue411\ue5fb\uc119\u1a0a\u52ef");
    ct3 = Util.toBytesFromUnicode("\u571b\u2420\u12fb\u7ae0\u7fa9\ubaac\u3df1\u02e0");

    pt4 = Util.toBytesFromUnicode("\uf69f\u2445\udf4f\u9b17\uad2b\u417b\ue66c\u3710");
    ct4 = Util.toBytesFromUnicode("\u08b0\ue279\u8859\u8881\ud920\ua9e6\u4f56\u15cd");

    ct = new byte[16];
    pt = new byte[16];

    mode = ModeFactory.getInstance(Registry.CBC_MODE, Registry.AES_CIPHER,
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
        harness.check(Arrays.equals(ct, ct1), "CBC-AES192-Decrypt block #1");

        mode.update(pt2, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct2), "CBC-AES192-Decrypt block #2");

        mode.update(pt3, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct3), "CBC-AES192-Decrypt block #3");

        mode.update(pt4, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct4), "CBC-AES192-Decrypt block #4");

        // decryption ........................................................
        mode.reset();
        attributes.put(IMode.STATE, new Integer(IMode.DECRYPTION));
        mode.init(attributes);

        mode.update(ct1, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt1), "CBC-AES192-Decrypt block #1");

        mode.update(ct2, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt2), "CBC-AES192-Decrypt block #2");

        mode.update(ct3, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt3), "CBC-AES192-Decrypt block #3");

        mode.update(ct4, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt4), "CBC-AES192-Decrypt block #4");

      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfCBC.testAES192");
      }

    harness.checkPoint("TestOfCBC.testAES256");
    /** F.2.5 CBC-AES256-Encrypt and F.2.6 CBC-AES256-Decrypt. */
    key = Util.toBytesFromUnicode("\u603d\ueb10\u15ca\u71be\u2b73\uaef0\u857d\u7781"
                                  + "\u1f35\u2c07\u3b61\u08d7\u2d98\u10a3\u0914\udff4");
    iv = Util.toBytesFromUnicode("\u0001\u0203\u0405\u0607\u0809\u0a0b\u0c0d\u0e0f");

    pt1 = Util.toBytesFromUnicode("\u6bc1\ubee2\u2e40\u9f96\ue93d\u7e11\u7393\u172a");
    ct1 = Util.toBytesFromUnicode("\uf58c\u4c04\ud6e5\uf1ba\u779e\uabfb\u5f7b\ufbd6");

    pt2 = Util.toBytesFromUnicode("\uae2d\u8a57\u1e03\uac9c\u9eb7\u6fac\u45af\u8e51");
    ct2 = Util.toBytesFromUnicode("\u9cfc\u4e96\u7edb\u808d\u679f\u777b\uc670\u2c7d");

    pt3 = Util.toBytesFromUnicode("\u30c8\u1c46\ua35c\ue411\ue5fb\uc119\u1a0a\u52ef");
    ct3 = Util.toBytesFromUnicode("\u39f2\u3369\ua9d9\ubacf\ua530\ue263\u0423\u1461");

    pt4 = Util.toBytesFromUnicode("\uf69f\u2445\udf4f\u9b17\uad2b\u417b\ue66c\u3710");
    ct4 = Util.toBytesFromUnicode("\ub2eb\u05e2\uc39b\ue9fc\uda6c\u1907\u8c6a\u9d1b");

    ct = new byte[16];
    pt = new byte[16];

    mode = ModeFactory.getInstance(Registry.CBC_MODE, Registry.AES_CIPHER,
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
        harness.check(Arrays.equals(ct, ct1), "CBC-AES256-Decrypt block #1");

        mode.update(pt2, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct2), "CBC-AES256-Decrypt block #2");

        mode.update(pt3, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct3), "CBC-AES256-Decrypt block #3");

        mode.update(pt4, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct4), "CBC-AES256-Decrypt block #4");

        // decryption ........................................................
        mode.reset();
        attributes.put(IMode.STATE, new Integer(IMode.DECRYPTION));
        mode.init(attributes);

        mode.update(ct1, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt1), "CBC-AES256-Decrypt block #1");

        mode.update(ct2, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt2), "CBC-AES256-Decrypt block #2");

        mode.update(ct3, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt3), "CBC-AES256-Decrypt block #3");

        mode.update(ct4, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt4), "CBC-AES256-Decrypt block #4");

      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfCBC.testAES256");
      }
  }
}