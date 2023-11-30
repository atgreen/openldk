/* TestOfTripleDESKeyWrap.java
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

import gnu.javax.crypto.cipher.TripleDES;
import gnu.javax.crypto.kwa.IKeyWrappingAlgorithm;
import gnu.javax.crypto.kwa.KeyWrappingAlgorithmFactory;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

/**
 * Conformance test of the RFC3217 Triple DES Key Wrapping Algorithm
 * implementation.
 */
public class TestOfTripleDESKeyWrap
    implements Testlet
{
  /* (non-Javadoc)
   * @see gnu.testlet.Testlet#test(gnu.testlet.TestHarness)
   */
  public void test(TestHarness harness)
  {
    testMethods(harness);
    testSymmetry(harness);
  }

  private void testMethods(TestHarness harness)
  {
    byte[] kek = new byte[24];
    try
      {
        IKeyWrappingAlgorithm kwa = KeyWrappingAlgorithmFactory.getInstance("kw-tripledes");
        Map attributes = new HashMap();
        attributes.put(IKeyWrappingAlgorithm.KEY_ENCRYPTION_KEY_MATERIAL, kek);
        kwa.init(attributes);

        String msg;
        byte[] km = new byte[24];
        msg = "Input length for wrapping MUST be 16 or 24 bytes";
        try
          {
            kwa.wrap(km, 0, 15);
            harness.fail(msg);
          }
        catch (IllegalArgumentException e)
          {
            harness.check(true, msg);
          }

        msg = "Input length for unwrapping MUST be 40 bytes";
        try
          {
            kwa.unwrap(km, 0, 24);
            harness.fail(msg);
          }
        catch (IllegalArgumentException e)
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

  private void testSymmetry(TestHarness harness)
  {
    byte[] kek = new byte[] {
        (byte) 0x25, (byte) 0x5e, (byte) 0x0d, (byte) 0x1c,
        (byte) 0x07, (byte) 0xb6, (byte) 0x46, (byte) 0xdf,
        (byte) 0xb3, (byte) 0x13, (byte) 0x4c, (byte) 0xc8,
        (byte) 0x43, (byte) 0xba, (byte) 0x8a, (byte) 0xa7,
        (byte) 0x1f, (byte) 0x02, (byte) 0x5b, (byte) 0x7c,
        (byte) 0x08, (byte) 0x38, (byte) 0x25, (byte) 0x1f };
    byte[] km = new byte[] { 
        (byte) 0x29, (byte) 0x23, (byte) 0xbf, (byte) 0x85,
        (byte) 0xe0, (byte) 0x6d, (byte) 0xd6, (byte) 0xae,
        (byte) 0x52, (byte) 0x91, (byte) 0x49, (byte) 0xf1,
        (byte) 0xf1, (byte) 0xba, (byte) 0xe9, (byte) 0xea,
        (byte) 0xb3, (byte) 0xa7, (byte) 0xda, (byte) 0x3d,
        (byte) 0x86, (byte) 0x0d, (byte) 0x3e, (byte) 0x98 };
    try
      {
        TripleDES.adjustParity(km, 0);

        IKeyWrappingAlgorithm kwa = KeyWrappingAlgorithmFactory.getInstance("kw-tripledes");
        Map attributes = new HashMap();
        attributes.put(IKeyWrappingAlgorithm.KEY_ENCRYPTION_KEY_MATERIAL, kek);
        kwa.init(attributes);

        byte[] wrapped = kwa.wrap(km, 0, km.length);
        harness.check(wrapped.length == 40,
                      "Wrapped key material MUST produce 40 bytes");

        byte[] unwrapped = kwa.unwrap(wrapped, 0, wrapped.length);
        harness.check(unwrapped.length == 24,
                      "Unwrapped key material MUST produce 24 bytes");
        harness.check(Arrays.equals(km, unwrapped),
                      "Unwrapped key material MUST match original value");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("testSymmetry(): " + x);
      }
  }
}
