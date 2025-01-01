/* TestOfCipherWrapUnwrap.java
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

import gnu.javax.crypto.cipher.TripleDES;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.security.InvalidKeyException;
import java.security.Key;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;

import javax.crypto.Cipher;
import javax.crypto.spec.SecretKeySpec;

/**
 * Conformance tests for the JCE KeyWrappingAlgorithmAdapter methods and
 * concrete implementations.
 */
public class TestOfCipherWrapUnwrap
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

  public void test(TestHarness harness)
  {
    testNames(harness);
    testMethods(harness);
    testAES128Wrap(harness);
    testAES192Wrap(harness);
    testAES256Wrap(harness);
    testTripleDESWrap(harness);
  }

  private void testNames(TestHarness harness)
  {
    harness.checkPoint("testNames()");
    try
      {
        String transform;

        transform = "kw-aes128";
        harness.check(mustInstantiate(harness, transform),
                      "MUST instantiate " + transform);
        transform += "/ecb/nopadding";
        harness.check(mustInstantiate(harness, transform),
                      "MUST instantiate " + transform);

        transform = "kw-aes192";
        harness.check(mustInstantiate(harness, transform),
                      "MUST instantiate " + transform);
        transform += "/ecb/nopadding";
        harness.check(mustInstantiate(harness, transform),
                      "MUST instantiate " + transform);

        transform = "kw-aes256";
        harness.check(mustInstantiate(harness, transform),
                      "MUST instantiate " + transform);
        transform += "/ecb/nopadding";
        harness.check(mustInstantiate(harness, transform),
                      "MUST instantiate " + transform);

        transform = "kw-tripledes";
        harness.check(mustInstantiate(harness, transform),
                      "MUST instantiate " + transform);
        transform += "/cbc/nopadding";
        harness.check(mustInstantiate(harness, transform),
                      "MUST instantiate " + transform);

        transform = "kw-aes128/cbc/nopadding";
        harness.check(mustNotInstantiate(harness, transform),
                      "MUST NOT instantiate " + transform);
        transform = "kw-aes192/cbc/nopadding";
        harness.check(mustNotInstantiate(harness, transform),
                      "MUST NOT instantiate " + transform);
        transform = "kw-aes256/cbc/nopadding";
        harness.check(mustNotInstantiate(harness, transform),
                      "MUST NOT instantiate " + transform);
        transform = "kw-tripledes/ecb/nopadding";
        harness.check(mustNotInstantiate(harness, transform),
                      "MUST NOT instantiate " + transform);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("testNames(): " + x);
      }
  }

  private void testMethods(TestHarness harness)
  {
    harness.checkPoint("testMethods()");
    try
      {
        Cipher kwa = Cipher.getInstance("kw-aes128");
        SecretKeySpec kek128 = new SecretKeySpec(new byte[16], "AES");
        SecretKeySpec kek192 = new SecretKeySpec(new byte[24], "AES");
        String msg;

        msg = "MUST NOT be able to call init() with a KEK > algorithm's key-size";
        try
          {
            kwa.init(Cipher.WRAP_MODE, kek192);
            harness.fail(msg);
          }
        catch (InvalidKeyException x)
          {
            harness.check(true, msg);
          }
        
        msg = "MUST be able to call init() with the right key-size KEK";
        try
          {
            kwa.init(Cipher.WRAP_MODE, kek128);
            harness.check(true, msg);
          }
        catch (RuntimeException x)
          {
            harness.fail(msg);
          }
        msg = "MUST be able to call getBlockSize()";
        try
          {
            kwa.getBlockSize();
            harness.check(true, msg);
          }
        catch (RuntimeException x)
          {
            harness.fail(msg);
          }
        msg = "MUST be able to call getIV()";
        try
          {
            kwa.getIV();
            harness.check(true, msg);
          }
        catch (RuntimeException x)
          {
            harness.fail(msg);
          }
        msg = "MUST NOT be able to call update()";
        try
          {
            kwa.update(KM128);
            harness.fail(msg);
          }
        catch (RuntimeException x)
          {
            harness.check(true, msg);
          }
        SecretKeySpec km128 = new SecretKeySpec(KM128, "AES");
        msg = "MUST be able to call wrap()";
        byte[] cipherText = null;
        try
          {
            cipherText = kwa.wrap(km128);
            harness.check(true, msg);
          }
        catch (RuntimeException x)
          {
            harness.fail(msg);
          }
        kwa.init(Cipher.UNWRAP_MODE, kek128);
        msg = "MUST NOT be able to call unwrap() with a different 'wrappedKeyType'";
        try
          {
            kwa.unwrap(cipherText, "AES", Cipher.PRIVATE_KEY);
            harness.fail(msg);
          }
        catch (NoSuchAlgorithmException x)
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

  private void testAES128Wrap(TestHarness harness)
  {
    harness.checkPoint("testAES128Wrap()");
    try
      {
        Cipher kwa = Cipher.getInstance("kw-aes128");
        byte[] kekBytes = new byte[16];
        for (int i = 0; i < kekBytes.length; i++)
          kekBytes[i] = (byte) i;
        SecretKeySpec kek = new SecretKeySpec(kekBytes, "AES");
        kwa.init(Cipher.WRAP_MODE, kek);
        SecretKeySpec km = new SecretKeySpec(KM128, "AES");
        byte[] cipherText = kwa.wrap(km);
        harness.check(Arrays.equals(cipherText, KM128_WRAPPED128),
                      "128-bit key material wrapped w/ 128-bit KEK MUST match "
                      + "expected value");

        kwa.init(Cipher.UNWRAP_MODE, kek);
        Key k = kwa.unwrap(cipherText, "AES", Cipher.SECRET_KEY);
        harness.check(km.equals(k),
                      "Unwrapped and original key-material MUST match");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("testAES128Wrap(): " + x);
      }
  }

  private void testAES192Wrap(TestHarness harness)
  {
    harness.checkPoint("testAES192Wrap()");
    try
      {
        Cipher kwa = Cipher.getInstance("kw-aes192");
        byte[] kekBytes = new byte[24];
        for (int i = 0; i < kekBytes.length; i++)
          kekBytes[i] = (byte) i;
        SecretKeySpec kek = new SecretKeySpec(kekBytes, "AES");
        kwa.init(Cipher.WRAP_MODE, kek);
        SecretKeySpec km = new SecretKeySpec(KM192, "AES");
        byte[] cipherText = kwa.wrap(km);
        harness.check(Arrays.equals(cipherText, KM192_WRAPPED192),
                      "192-bit key material wrapped w/ 192-bit KEK MUST match "
                      + "expected value");

        kwa.init(Cipher.UNWRAP_MODE, kek);
        Key k = kwa.unwrap(cipherText, "AES", Cipher.SECRET_KEY);
        harness.check(km.equals(k),
                      "Unwrapped and original key-material MUST match");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("testAES192Wrap(): " + x);
      }
  }

  private void testAES256Wrap(TestHarness harness)
  {
    harness.checkPoint("testAES256Wrap()");
    try
      {
        Cipher kwa = Cipher.getInstance("kw-aes256");
        byte[] kekBytes = new byte[32];
        for (int i = 0; i < kekBytes.length; i++)
          kekBytes[i] = (byte) i;
        SecretKeySpec kek = new SecretKeySpec(kekBytes, "AES");
        kwa.init(Cipher.WRAP_MODE, kek);
        SecretKeySpec km = new SecretKeySpec(KM256, "AES");
        byte[] cipherText = kwa.wrap(km);
        harness.check(Arrays.equals(cipherText, KM256_WRAPPED256),
                      "256-bit key material wrapped w/ 256-bit KEK MUST match "
                      + "expected value");

        kwa.init(Cipher.UNWRAP_MODE, kek);
        Key k = kwa.unwrap(cipherText, "AES", Cipher.SECRET_KEY);
        harness.check(km.equals(k),
                      "Unwrapped and original key-material MUST match");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("testAES256Wrap(): " + x);
      }
  }

  private void testTripleDESWrap(TestHarness harness)
  {
    harness.checkPoint("testTripleDESWrap()");
    try
      {
        Cipher kwa = Cipher.getInstance("kw-tripledes");
        byte[] kekBytes = new byte[24];
        for (int i = 0; i < kekBytes.length; i++)
          kekBytes[i] = (byte) i;
        SecretKeySpec kek = new SecretKeySpec(kekBytes, "TripleDES");
        kwa.init(Cipher.WRAP_MODE, kek);
        byte[] kmBytes = new byte[24];
        for (int i = 0; i < kmBytes.length; i++)
          kmBytes[i] = (byte)(i + 1);
        SecretKeySpec km = new SecretKeySpec(kmBytes, "TripleDES");
        byte[] cipherText = kwa.wrap(km);

        kwa.init(Cipher.UNWRAP_MODE, kek);
        Key k = kwa.unwrap(cipherText, "TripleDES", Cipher.SECRET_KEY);
        // key-unwrap ALWAYS returns a parity-adjusted (proper!) key
        TripleDES.adjustParity(kmBytes, 0);
        SecretKeySpec kmpa = new SecretKeySpec(kmBytes, "TripleDES");
        harness.check(kmpa.equals(k),
                      "Unwrapped and original key-material MUST match");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("testTripleDESWrap(): " + x);
      }
  }

  private boolean mustInstantiate(TestHarness harness, String transform)
  {
    Cipher result = null;
    try
      {
        result = Cipher.getInstance(transform);
      }
    catch (Exception x)
      {
        harness.debug(x);
      }
    return result != null;
  }

  private boolean mustNotInstantiate(TestHarness harness, String transform)
  {
    Cipher result = null;
    try
      {
        result = Cipher.getInstance(transform);
      }
    catch (Exception x)
      {
        harness.debug(x);
      }
    return result == null;
  }
}
