/* TestOfECB.java -- 
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
 * Conformance tests of the ECB implementation.
 *
 * <p>References:</p>
 * <ol>
 *    <li><a href="http://csrc.nist.gov/encryption/modes/Recommendation/Modes01.pdf">
 *    Recommendation for Block Cipher Modes of Operation Methods and Techniques</a>,
 *    Morris Dworkin.</li>
 * </ol>
 */
public class TestOfECB implements Testlet
{
  private byte[] key, pt1, ct1, pt2, ct2, pt3, ct3, pt4, ct4, pt, ct;

  private IMode mode;

  private Map attributes = new HashMap();

  public void test(TestHarness harness)
  {
    harness.checkPoint("TestOfECB.testAES128");
    /** F.1.1 ECB-AES128-Encrypt and F.1.2 ECB-AES128-Decrypt. */
    key = Util.toBytesFromUnicode("\u2b7e\u1516\u28ae\ud2a6\uabf7\u1588\u09cf\u4f3c");

    pt1 = Util.toBytesFromUnicode("\u6bc1\ubee2\u2e40\u9f96\ue93d\u7e11\u7393\u172a");
    ct1 = Util.toBytesFromUnicode("\u3ad7\u7bb4\u0d7a\u3660\ua89e\ucaf3\u2466\uef97");

    pt2 = Util.toBytesFromUnicode("\uae2d\u8a57\u1e03\uac9c\u9eb7\u6fac\u45af\u8e51");
    ct2 = Util.toBytesFromUnicode("\uf5d3\ud585\u03b9\u699d\ue785\u895a\u96fd\ubaaf");

    pt3 = Util.toBytesFromUnicode("\u30c8\u1c46\ua35c\ue411\ue5fb\uc119\u1a0a\u52ef");
    ct3 = Util.toBytesFromUnicode("\u43b1\ucd7f\u598e\uce23\u881b\u00e3\ued03\u0688");

    pt4 = Util.toBytesFromUnicode("\uf69f\u2445\udf4f\u9b17\uad2b\u417b\ue66c\u3710");
    ct4 = Util.toBytesFromUnicode("\u7b0c\u785e\u27e8\uad3f\u8223\u2071\u0472\u5dd4");

    ct = new byte[16];
    pt = new byte[16];

    mode = ModeFactory.getInstance(Registry.ECB_MODE, Registry.AES_CIPHER,
                                   128 / 8);
    attributes.clear();
    attributes.put(IMode.KEY_MATERIAL, key);
    try
      {
        // encryption ........................................................
        attributes.put(IMode.STATE, new Integer(IMode.ENCRYPTION));
        mode.init(attributes);

        mode.update(pt1, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct1), "ECB-AES128-Decrypt block #1");

        mode.update(pt2, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct2), "ECB-AES128-Decrypt block #2");

        mode.update(pt3, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct3), "ECB-AES128-Decrypt block #3");

        mode.update(pt4, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct4), "ECB-AES128-Decrypt block #4");

        // decryption ........................................................
        mode.reset();
        attributes.put(IMode.STATE, new Integer(IMode.DECRYPTION));
        mode.init(attributes);

        mode.update(ct1, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt1), "ECB-AES128-Decrypt block #1");

        mode.update(ct2, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt2), "ECB-AES128-Decrypt block #2");

        mode.update(ct3, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt3), "ECB-AES128-Decrypt block #3");

        mode.update(ct4, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt4), "ECB-AES128-Decrypt block #4");

      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfECB.testAES128");
      }

    harness.checkPoint("TestOfECB.testAES192");
    /** F.1.3 ECB-AES192-Encrypt and F.1.4 ECB-AES192-Decrypt. */
    key = Util.toBytesFromUnicode("\u8e73\ub0f7\uda0e\u6452\uc810\uf32b\u8090\u79e5"
                                  + "\u62f8\uead2\u522c\u6b7b");

    pt1 = Util.toBytesFromUnicode("\u6bc1\ubee2\u2e40\u9f96\ue93d\u7e11\u7393\u172a");
    ct1 = Util.toBytesFromUnicode("\ubd33\u4f1d\u6e45\uf25f\uf712\ua214\u571f\ua5cc");

    pt2 = Util.toBytesFromUnicode("\uae2d\u8a57\u1e03\uac9c\u9eb7\u6fac\u45af\u8e51");
    ct2 = Util.toBytesFromUnicode("\u9741\u0484\u6d0a\ud3ad\u7734\uecb3\uecee\u4eef");

    pt3 = Util.toBytesFromUnicode("\u30c8\u1c46\ua35c\ue411\ue5fb\uc119\u1a0a\u52ef");
    ct3 = Util.toBytesFromUnicode("\uef7a\ufd22\u70e2\ue60a\udce0\uba2f\uace6\u444e");

    pt4 = Util.toBytesFromUnicode("\uf69f\u2445\udf4f\u9b17\uad2b\u417b\ue66c\u3710");
    ct4 = Util.toBytesFromUnicode("\u9a4b\u41ba\u738d\u6c72\ufb16\u6916\u03c1\u8e0e");

    ct = new byte[16];
    pt = new byte[16];

    mode = ModeFactory.getInstance(Registry.ECB_MODE, Registry.AES_CIPHER,
                                   128 / 8);
    attributes.clear();
    attributes.put(IMode.KEY_MATERIAL, key);
    try
      {
        // encryption ........................................................
        attributes.put(IMode.STATE, new Integer(IMode.ENCRYPTION));
        mode.init(attributes);

        mode.update(pt1, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct1), "ECB-AES192-Decrypt block #1");

        mode.update(pt2, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct2), "ECB-AES192-Decrypt block #2");

        mode.update(pt3, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct3), "ECB-AES192-Decrypt block #3");

        mode.update(pt4, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct4), "ECB-AES192-Decrypt block #4");

        // decryption ........................................................
        mode.reset();
        attributes.put(IMode.STATE, new Integer(IMode.DECRYPTION));
        mode.init(attributes);

        mode.update(ct1, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt1), "ECB-AES192-Decrypt block #1");

        mode.update(ct2, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt2), "ECB-AES192-Decrypt block #2");

        mode.update(ct3, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt3), "ECB-AES192-Decrypt block #3");

        mode.update(ct4, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt4), "ECB-AES192-Decrypt block #4");

      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfECB.testAES192");
      }

    harness.checkPoint("TestOfECB.testAES256");
    /** F.1.5 ECB-AES256-Encrypt and F.1.6 ECB-AES256-Decrypt. */
    key = Util.toBytesFromUnicode("\u603d\ueb10\u15ca\u71be\u2b73\uaef0\u857d\u7781"
                                  + "\u1f35\u2c07\u3b61\u08d7\u2d98\u10a3\u0914\udff4");

    pt1 = Util.toBytesFromUnicode("\u6bc1\ubee2\u2e40\u9f96\ue93d\u7e11\u7393\u172a");
    ct1 = Util.toBytesFromUnicode("\uf3ee\ud1bd\ub5d2\ua03c\u064b\u5a7e\u3db1\u81f8");

    pt2 = Util.toBytesFromUnicode("\uae2d\u8a57\u1e03\uac9c\u9eb7\u6fac\u45af\u8e51");
    ct2 = Util.toBytesFromUnicode("\u591c\ucb10\ud410\ued26\udc5b\ua74a\u3136\u2870");

    pt3 = Util.toBytesFromUnicode("\u30c8\u1c46\ua35c\ue411\ue5fb\uc119\u1a0a\u52ef");
    ct3 = Util.toBytesFromUnicode("\ub6ed\u21b9\u9ca6\uf4f9\uf153\ue7b1\ubeaf\ued1d");

    pt4 = Util.toBytesFromUnicode("\uf69f\u2445\udf4f\u9b17\uad2b\u417b\ue66c\u3710");
    ct4 = Util.toBytesFromUnicode("\u2330\u4b7a\u39f9\uf3ff\u067d\u8d8f\u9e24\uecc7");

    ct = new byte[16];
    pt = new byte[16];

    mode = ModeFactory.getInstance(Registry.ECB_MODE, Registry.AES_CIPHER,
                                   128 / 8);
    attributes.clear();
    attributes.put(IMode.KEY_MATERIAL, key);
    try
      {
        // encryption ........................................................
        attributes.put(IMode.STATE, new Integer(IMode.ENCRYPTION));
        mode.init(attributes);

        mode.update(pt1, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct1), "ECB-AES256-Decrypt block #1");

        mode.update(pt2, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct2), "ECB-AES256-Decrypt block #2");

        mode.update(pt3, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct3), "ECB-AES256-Decrypt block #3");

        mode.update(pt4, 0, ct, 0);
        harness.check(Arrays.equals(ct, ct4), "ECB-AES256-Decrypt block #4");

        // decryption ........................................................
        mode.reset();
        attributes.put(IMode.STATE, new Integer(IMode.DECRYPTION));
        mode.init(attributes);

        mode.update(ct1, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt1), "ECB-AES256-Decrypt block #1");

        mode.update(ct2, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt2), "ECB-AES256-Decrypt block #2");

        mode.update(ct3, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt3), "ECB-AES256-Decrypt block #3");

        mode.update(ct4, 0, pt, 0);
        harness.check(Arrays.equals(pt, pt4), "ECB-AES256-Decrypt block #4");

      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfECB.testAES256");
      }
  }
}