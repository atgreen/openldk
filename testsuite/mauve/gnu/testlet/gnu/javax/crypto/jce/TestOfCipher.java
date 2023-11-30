/* TestOfCipher.java -- 
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

import gnu.java.security.Registry;
import gnu.javax.crypto.cipher.CipherFactory;
import gnu.javax.crypto.cipher.IBlockCipher;
import gnu.javax.crypto.jce.GnuCrypto;
import gnu.javax.crypto.mode.IMode;
import gnu.javax.crypto.mode.ModeFactory;
import gnu.javax.crypto.pad.IPad;
import gnu.javax.crypto.pad.PadFactory;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.security.Security;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;

import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;

/**
 * Conformance tests for the JCE Provider implementations of the Cipher SPI
 * classes.
 */
public class TestOfCipher implements Testlet
{
  public void test(TestHarness harness)
  {
    setUp();

    testUnknownCipher(harness);
    testEquality(harness);
    testPadding(harness);
    testPartial(harness);
    testDoFinal(harness);
  }

  /** Should fail with an unknown algorithm. */
  public void testUnknownCipher(TestHarness harness)
  {
    harness.checkPoint("testUnknownCipher");
    try
      {
        Cipher.getInstance("Godot", Registry.GNU_CRYPTO);
        harness.fail("testUnknownCipher()");
      }
    catch (Exception x)
      {
        harness.check(true);
      }
  }

  /**
   * Tests if the result of using a cipher through gnu.crypto Factory classes
   * yields same value as using instances obtained the JCE way.
   */
  public void testEquality(TestHarness harness)
  {
    harness.checkPoint("testEquality");
    String cipherName = null, modeName;
    IMode gnu = null;
    Cipher jce = null;
    HashMap attrib = new HashMap();
    byte[] pt = null;
    //      byte[] iv = null;
    byte[] ct1 = null, ct2 = null;
    byte[] cpt1 = null, cpt2 = null;
    Iterator ci, mi;
    int bs;
    try
      {
        for (ci = CipherFactory.getNames().iterator(); ci.hasNext();)
          {
            cipherName = (String) ci.next();
            IBlockCipher cipher = CipherFactory.getInstance(cipherName);
            bs = cipher.defaultBlockSize();
            for (mi = ModeFactory.getNames().iterator(); mi.hasNext();)
              {
                modeName = (String) mi.next();
                gnu = ModeFactory.getInstance(modeName, cipher, bs);
                jce = Cipher.getInstance(cipherName + "/" + modeName
                                         + "/NoPadding", Registry.GNU_CRYPTO);
                pt = new byte[bs];
                for (int i = 0; i < bs; i++)
                  {
                    pt[i] = (byte) i;
                  }
                attrib.put(IBlockCipher.CIPHER_BLOCK_SIZE, new Integer(bs));
                attrib.put(IMode.IV, pt);
                for (Iterator ks = cipher.keySizes(); ks.hasNext();)
                  {
                    byte[] kb = new byte[((Integer) ks.next()).intValue()];
                    for (int i = 0; i < kb.length; i++)
                      {
                        kb[i] = (byte) i;
                      }
                    attrib.put(IMode.STATE, new Integer(IMode.ENCRYPTION));
                    attrib.put(IBlockCipher.KEY_MATERIAL, kb);
                    gnu.reset();
                    gnu.init(attrib);
                    ct1 = new byte[bs];
                    gnu.update(pt, 0, ct1, 0);
                    jce.init(Cipher.ENCRYPT_MODE,
                             new SecretKeySpec(kb, cipherName),
                             new IvParameterSpec(pt));
                    ct2 = new byte[bs];
                    jce.doFinal(pt, 0, bs, ct2, 0);
                    harness.check(Arrays.equals(ct1, ct2), "testEquality("
                                                           + cipherName + ")");

                    attrib.put(IMode.STATE, new Integer(IMode.DECRYPTION));
                    cpt1 = new byte[bs];
                    gnu.reset();
                    gnu.init(attrib);
                    gnu.update(ct1, 0, cpt1, 0);
                    harness.check(Arrays.equals(pt, cpt1), "testEquality("
                                                           + cipherName + ")");

                    jce.init(Cipher.DECRYPT_MODE,
                             new SecretKeySpec(kb, cipherName),
                             new IvParameterSpec(pt));
                    cpt2 = new byte[bs];
                    jce.doFinal(ct2, 0, bs, cpt2, 0);
                    harness.check(Arrays.equals(pt, cpt2), "testEquality("
                                                           + cipherName + ")");

                    harness.check(Arrays.equals(cpt1, cpt2), "testEquality("
                                                             + cipherName + ")");
                  }
              }
          }
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("testEquality(" + cipherName + "): " + String.valueOf(x));
      }
  }

  /**
   * Test that the padding results in the same cipher/plaintexts for instances
   * derived from both the GNU Factory and the JCE one.
   */
  public void testPadding(TestHarness harness)
  {
    harness.checkPoint("testPadding");
    String padName = null;
    IMode gnu = ModeFactory.getInstance("ECB", "AES", 16);
    IPad pad;
    Cipher jce;
    byte[] kb = new byte[32];
    for (int i = 0; i < kb.length; i++)
      kb[i] = (byte) i;
    byte[] pt = new byte[42];
    for (int i = 0; i < pt.length; i++)
      pt[i] = (byte) i;
    byte[] ppt = new byte[48]; // padded plaintext.
    System.arraycopy(pt, 0, ppt, 0, 42);
    byte[] ct1 = new byte[48], ct2 = new byte[48];
    byte[] cpt1 = new byte[42], cpt2 = new byte[42];
    HashMap attrib = new HashMap();
    attrib.put(IBlockCipher.KEY_MATERIAL, kb);
    try
      {
        for (Iterator it = PadFactory.getNames().iterator(); it.hasNext();)
          {
            padName = (String) it.next();
            // skip EME-PKCS1-V1.5 padding since it's not a true block cipher
            // padding algorithm
            if (padName.equalsIgnoreCase(Registry.EME_PKCS1_V1_5_PAD))
              continue;

            pad = PadFactory.getInstance(padName);
            pad.reset();
            pad.init(16);

            byte[] padding = pad.pad(pt, 0, pt.length);
            System.arraycopy(padding, 0, ppt, 42, padding.length);
            attrib.put(IMode.STATE, new Integer(IMode.ENCRYPTION));
            gnu.reset();
            gnu.init(attrib);
            for (int i = 0; i < ppt.length; i += 16)
              gnu.update(ppt, i, ct1, i);

            jce = Cipher.getInstance("AES/ECB/" + padName, Registry.GNU_CRYPTO);
            jce.init(Cipher.DECRYPT_MODE, new SecretKeySpec(kb, "AES"));
            jce.doFinal(ct1, 0, ct1.length, cpt1, 0);
            harness.check(Arrays.equals(pt, cpt1),
                          "testPadding(" + padName + ")");

            jce.init(Cipher.ENCRYPT_MODE, new SecretKeySpec(kb, "AES"));
            jce.doFinal(pt, 0, pt.length, ct2, 0);

            attrib.put(IMode.STATE, new Integer(IMode.DECRYPTION));
            gnu.reset();
            gnu.init(attrib);
            byte[] pcpt = new byte[48];
            for (int i = 0; i < ct2.length; i += 16)
              gnu.update(ct2, i, pcpt, i);

            int trim = pad.unpad(pcpt, 0, pcpt.length);
            System.arraycopy(pcpt, 0, cpt2, 0, pcpt.length - trim);

            harness.check(Arrays.equals(pt, cpt2),
                          "testPadding(" + padName + ")");
          }
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("testPadding(" + padName + "): " + String.valueOf(x));
      }
  }

  /** Test the update() methods with incomplete blocks. */
  public void testPartial(TestHarness harness)
  {
    harness.checkPoint("testPartial");
    String cipherName = null;
    Cipher full, part1, part2;
    IBlockCipher gnu;
    byte[] pt;
    byte[] kb;
    byte[] ct1, ct2, ct3, ct4;
    int i, blockSize;
    try
      {
        for (Iterator it = CipherFactory.getNames().iterator(); it.hasNext();)
          {
            cipherName = (String) it.next();
            gnu = CipherFactory.getInstance(cipherName);
            full = Cipher.getInstance(cipherName, Registry.GNU_CRYPTO);
            part1 = Cipher.getInstance(cipherName, Registry.GNU_CRYPTO);
            part2 = Cipher.getInstance(cipherName, Registry.GNU_CRYPTO);
            //            pt = new byte[gnu.defaultBlockSize()];
            blockSize = gnu.defaultBlockSize();
            pt = new byte[2 * blockSize];
            for (i = 0; i < pt.length; i++)
              {
                pt[i] = (byte) i;
              }
            kb = new byte[gnu.defaultKeySize()];
            for (i = 0; i < kb.length; i++)
              {
                kb[i] = (byte) i;
              }
            full.init(Cipher.ENCRYPT_MODE, new SecretKeySpec(kb, cipherName));
            //            ct1 = full.doFinal(pt);
            ct1 = full.doFinal(pt, blockSize, blockSize);

            part1.init(Cipher.ENCRYPT_MODE, new SecretKeySpec(kb, cipherName));
            //            for (i = 0; i < pt.length - 1; i++) {
            for (i = blockSize; i < pt.length - 1; i++)
              {
                part1.update(pt, i, 1);
              }
            ct2 = part1.doFinal(pt, i, 1);

            harness.check(Arrays.equals(ct1, ct2), "testPartial1(" + cipherName
                                                   + ")");

            // this is tricky: only the update of the last byte should return
            // a full block.  also, the doFinal() should return an empty byte[]
            part2.init(Cipher.ENCRYPT_MODE, new SecretKeySpec(kb, cipherName));
            //            for (i = 0; i < pt.length - 1; i++) {
            for (i = blockSize; i < pt.length - 1; i++)
              {
                part2.update(pt, i, 1);
              }
            ct3 = part2.update(pt, i, 1);
            harness.check(Arrays.equals(ct3, ct2), "testPartial2(" + cipherName
                                                   + ")");

            ct4 = part2.doFinal();
            harness.check(ct4 != null && ct4.length == 0, "testPartial3("
                                                          + cipherName + ")");
          }
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("testPartial(" + cipherName + "): " + String.valueOf(x));
      }
  }

  /** doFinal() with a short block and no padding should invariably fail. */
  public void testDoFinal(TestHarness harness)
  {
    harness.checkPoint("testDoFinal");
    String cipherName = null;
    Cipher jce;
    IBlockCipher gnu;
    byte[] pt;
    byte[] kb;
    Iterator it;
    for (it = CipherFactory.getNames().iterator(); it.hasNext();)
      {
        try
          {
            cipherName = (String) it.next();
            gnu = CipherFactory.getInstance(cipherName);
            jce = Cipher.getInstance(cipherName, Registry.GNU_CRYPTO);
            pt = new byte[gnu.defaultBlockSize() - 1];
            for (int i = 0; i < pt.length; i++)
              {
                pt[i] = (byte) i;
              }
            kb = new byte[gnu.defaultKeySize()];
            for (int i = 0; i < kb.length; i++)
              {
                kb[i] = (byte) i;
              }
            jce.init(Cipher.ENCRYPT_MODE, new SecretKeySpec(kb, cipherName));
            jce.doFinal(pt);
            harness.fail("testDoFinal(" + cipherName + ")");
          }
        catch (IllegalBlockSizeException ibse)
          {
            harness.check(true, "testDoFinal(" + cipherName + ")");
          }
        catch (Exception x)
          {
            harness.debug(x);
            harness.fail("testDoFinal(" + cipherName + "): "
                         + String.valueOf(x));
          }
      }
  }

  private void setUp()
  {
    Security.addProvider(new GnuCrypto());
  }
}