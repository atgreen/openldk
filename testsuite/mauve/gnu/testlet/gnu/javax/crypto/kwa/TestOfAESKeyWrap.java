/* TestOfAESKeyWrap.java
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

package gnu.testlet.gnu.javax.crypto.kwa;

import gnu.javax.crypto.kwa.IKeyWrappingAlgorithm;
import gnu.javax.crypto.kwa.KeyWrappingAlgorithmFactory;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import javax.crypto.ShortBufferException;

/**
 * Conformance test of the RFC3394 Key Wrapping Algorithm implementation. Test
 * vectors are from RFC-3394.
 */
public class TestOfAESKeyWrap
    implements Testlet
{
  private static final byte[] KM128 = new byte[] {
      (byte) 0x00, (byte) 0x11, (byte) 0x22, (byte) 0x33,
      (byte) 0x44, (byte) 0x55, (byte) 0x66, (byte) 0x77,
      (byte) 0x88, (byte) 0x99, (byte) 0xAA, (byte) 0xBB,
      (byte) 0xCC, (byte) 0xDD, (byte) 0xEE, (byte) 0xFF };
  private static final byte[] KM128_WRAPPED128 = new byte[] {
      (byte) 0x1F, (byte) 0xA6, (byte) 0x8B, (byte) 0x0A,
      (byte) 0x81, (byte) 0x12, (byte) 0xB4, (byte) 0x47,
      (byte) 0xAE, (byte) 0xF3, (byte) 0x4B, (byte) 0xD8,
      (byte) 0xFB, (byte) 0x5A, (byte) 0x7B, (byte) 0x82,
      (byte) 0x9D, (byte) 0x3E, (byte) 0x86, (byte) 0x23,
      (byte) 0x71, (byte) 0xD2, (byte) 0xCF, (byte) 0xE5 };
  private static final byte[] KM128_WRAPPED192 = new byte[] {
      (byte) 0x96, (byte) 0x77, (byte) 0x8B, (byte) 0x25,
      (byte) 0xAE, (byte) 0x6C, (byte) 0xA4, (byte) 0x35,
      (byte) 0xF9, (byte) 0x2B, (byte) 0x5B, (byte) 0x97,
      (byte) 0xC0, (byte) 0x50, (byte) 0xAE, (byte) 0xD2,
      (byte) 0x46, (byte) 0x8A, (byte) 0xB8, (byte) 0xA1,
      (byte) 0x7A, (byte) 0xD8, (byte) 0x4E, (byte) 0x5D };
  private static final byte[] KM128_WRAPPED256 = new byte[] {
      (byte) 0x64, (byte) 0xE8, (byte) 0xC3, (byte) 0xF9,
      (byte) 0xCE, (byte) 0x0F, (byte) 0x5B, (byte) 0xA2,
      (byte) 0x63, (byte) 0xE9, (byte) 0x77, (byte) 0x79,
      (byte) 0x05, (byte) 0x81, (byte) 0x8A, (byte) 0x2A,
      (byte) 0x93, (byte) 0xC8, (byte) 0x19, (byte) 0x1E,
      (byte) 0x7D, (byte) 0x6E, (byte) 0x8A, (byte) 0xE7 };
  private static final byte[] KM192 = new byte[] {
      (byte) 0x00, (byte) 0x11, (byte) 0x22, (byte) 0x33,
      (byte) 0x44, (byte) 0x55, (byte) 0x66, (byte) 0x77,
      (byte) 0x88, (byte) 0x99, (byte) 0xAA, (byte) 0xBB,
      (byte) 0xCC, (byte) 0xDD, (byte) 0xEE, (byte) 0xFF,
      (byte) 0x00, (byte) 0x01, (byte) 0x02, (byte) 0x03,
      (byte) 0x04, (byte) 0x05, (byte) 0x06, (byte) 0x07 };
  private static final byte[] KM192_WRAPPED192 = new byte[] {
      (byte) 0x03, (byte) 0x1D, (byte) 0x33, (byte) 0x26,
      (byte) 0x4E, (byte) 0x15, (byte) 0xD3, (byte) 0x32,
      (byte) 0x68, (byte) 0xF2, (byte) 0x4E, (byte) 0xC2,
      (byte) 0x60, (byte) 0x74, (byte) 0x3E, (byte) 0xDC,
      (byte) 0xE1, (byte) 0xC6, (byte) 0xC7, (byte) 0xDD,
      (byte) 0xEE, (byte) 0x72, (byte) 0x5A, (byte) 0x93,
      (byte) 0x6B, (byte) 0xA8, (byte) 0x14, (byte) 0x91,
      (byte) 0x5C, (byte) 0x67, (byte) 0x62, (byte) 0xD2 };
  private static final byte[] KM192_WRAPPED256 = new byte[] {
      (byte) 0xA8, (byte) 0xF9, (byte) 0xBC, (byte) 0x16,
      (byte) 0x12, (byte) 0xC6, (byte) 0x8B, (byte) 0x3F,
      (byte) 0xF6, (byte) 0xE6, (byte) 0xF4, (byte) 0xFB,
      (byte) 0xE3, (byte) 0x0E, (byte) 0x71, (byte) 0xE4,
      (byte) 0x76, (byte) 0x9C, (byte) 0x8B, (byte) 0x80,
      (byte) 0xA3, (byte) 0x2C, (byte) 0xB8, (byte) 0x95,
      (byte) 0x8C, (byte) 0xD5, (byte) 0xD1, (byte) 0x7D,
      (byte) 0x6B, (byte) 0x25, (byte) 0x4D, (byte) 0xA1 };
  private static final byte[] KM256 = new byte[] {
      (byte) 0x00, (byte) 0x11, (byte) 0x22, (byte) 0x33,
      (byte) 0x44, (byte) 0x55, (byte) 0x66, (byte) 0x77,
      (byte) 0x88, (byte) 0x99, (byte) 0xAA, (byte) 0xBB,
      (byte) 0xCC, (byte) 0xDD, (byte) 0xEE, (byte) 0xFF,
      (byte) 0x00, (byte) 0x01, (byte) 0x02, (byte) 0x03,
      (byte) 0x04, (byte) 0x05, (byte) 0x06, (byte) 0x07,
      (byte) 0x08, (byte) 0x09, (byte) 0x0A, (byte) 0x0B,
      (byte) 0x0C, (byte) 0x0D, (byte) 0x0E, (byte) 0x0F };
  private static final byte[] KM256_WRAPPED256 = new byte[] {
      (byte) 0x28, (byte) 0xC9, (byte) 0xF4, (byte) 0x04,
      (byte) 0xC4, (byte) 0xB8, (byte) 0x10, (byte) 0xF4,
      (byte) 0xCB, (byte) 0xCC, (byte) 0xB3, (byte) 0x5C,
      (byte) 0xFB, (byte) 0x87, (byte) 0xF8, (byte) 0x26,
      (byte) 0x3F, (byte) 0x57, (byte) 0x86, (byte) 0xE2,
      (byte) 0xD8, (byte) 0x0E, (byte) 0xD3, (byte) 0x26,
      (byte) 0xCB, (byte) 0xC7, (byte) 0xF0, (byte) 0xE7,
      (byte) 0x1A, (byte) 0x99, (byte) 0xF4, (byte) 0x3B,
      (byte) 0xFB, (byte) 0x98, (byte) 0x8B, (byte) 0x9B,
      (byte) 0x7A, (byte) 0x02, (byte) 0xDD, (byte) 0x21 };

  /* (non-Javadoc)
   * @see gnu.testlet.Testlet#test(gnu.testlet.TestHarness)
   */
  public void test(TestHarness harness)
  {
    testMethods(harness);
    test64BitBlock(harness);
    testKek(harness, 128, KM128, KM128_WRAPPED128);
    testKek(harness, 192, KM128, KM128_WRAPPED192);
    testKek(harness, 256, KM128, KM128_WRAPPED256);
    testKek(harness, 192, KM192, KM192_WRAPPED192);
    testKek(harness, 256, KM192, KM192_WRAPPED256);
    testKek(harness, 256, KM256, KM256_WRAPPED256);
  }

  private void testMethods(TestHarness harness)
  {
    byte[] kek = new byte[16];
    try
      {
        IKeyWrappingAlgorithm kwa = KeyWrappingAlgorithmFactory.getInstance("kw-aes");
        Map attributes = new HashMap();
        attributes.put(IKeyWrappingAlgorithm.KEY_ENCRYPTION_KEY_MATERIAL, kek);
        kwa.init(attributes);

        String msg;
        byte[] km1 = new byte[24];
        msg = "Input length MUST be a multiple of 8 bytes";
        try
          {
            kwa.wrap(km1, 0, 17, km1, 0);
            harness.fail(msg);
          }
        catch (IllegalArgumentException e)
          {
            harness.check(true, msg);
          }

        msg = "Output length MUST be at least 8 bytes larger than input length";
        try
          {
            kwa.wrap(km1, 0, 16, km1, 1);
            harness.fail(msg);
          }
        catch (ShortBufferException e)
          {
            harness.check(true, msg);
          }
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("testMethods(): " + x);
      }
  }

  private void test64BitBlock(TestHarness harness)
  {
    byte[] kek = new byte[16];
    for (int i = 0; i < 16; i++)
      kek[i] = (byte) i;

    byte[] km = new byte[] { 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07 };
    try
      {
        IKeyWrappingAlgorithm kwa = KeyWrappingAlgorithmFactory.getInstance("kw-aes");
        Map attributes = new HashMap();
        attributes.put(IKeyWrappingAlgorithm.KEY_ENCRYPTION_KEY_MATERIAL, kek);
        kwa.init(attributes);

        byte[] wrapped = new byte[km.length + 8];
        int outputLength = kwa.wrap(km, 0, km.length, wrapped, 0);
        harness.check(outputLength == 16,
                      "Wrapped 64-bit key material MUST produce 16 bytes");

        byte[] unwrapped = new byte[8];
        outputLength = kwa.unwrap(wrapped, 0, wrapped.length, unwrapped, 0);
        harness.check(outputLength == 8,
                      "Unwrapped 128-bit key material MUST produce 8 bytes");
        harness.check(Arrays.equals(km, unwrapped),
                      "Unwrapped key material MUST match original value");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("testKek(): " + x);
      }
  }

  private void testKek(TestHarness harness, int keyLength,
                       byte[] keyMaterial, byte[] wrappedKeyMaterial)
  {
    int limit = keyLength / 8;
    byte[] kek = new byte[limit];
    for (int i = 0; i < limit; i++)
      kek[i] = (byte) i;

    int keyMaterialLength = keyMaterial.length * 8;
    try
      {
        IKeyWrappingAlgorithm kwa = KeyWrappingAlgorithmFactory.getInstance("kw-aes");
        Map attributes = new HashMap();
        attributes.put(IKeyWrappingAlgorithm.KEY_ENCRYPTION_KEY_MATERIAL, kek);
        kwa.init(attributes);

        byte[] km = (byte[]) keyMaterial.clone();
        byte[] wrapped = new byte[km.length + 8];
        kwa.wrap(km, 0, km.length, wrapped, 0);
        harness.check(Arrays.equals(wrappedKeyMaterial, wrapped),
                      keyMaterialLength + "-bit key material wrapped w/ "
                      + keyLength + "-bit KEK MUST match expected value");

        byte[] unwrapped = new byte[wrappedKeyMaterial.length - 8];
        kwa.unwrap(wrappedKeyMaterial, 0, wrappedKeyMaterial.length, unwrapped, 0);
        harness.check(Arrays.equals(keyMaterial, unwrapped),
                      keyMaterialLength + "-bit key material unwrapped w/ "
                      + keyLength + "-bit KEK MUST match expected value");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("testKek(): " + x);
      }
  }
}
