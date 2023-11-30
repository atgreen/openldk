/* ctor.java
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

// Tags: JDK1.2

package gnu.testlet.java.math.BigInteger;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.math.BigInteger;
import java.util.Random;

/**
 * Conformance tests for some of BigInteger constructors.
 */
public class ctor
    implements Testlet
{
  public void test(TestHarness harness)
  {
    testCtorStringInt(harness);
    testCtorIntIntRandom(harness);
    testCtorIntRandom(harness);
    testCtorByteArray(harness);
  }

  private void testCtorStringInt(TestHarness harness)
  {
    harness.checkPoint("testCtorStringInt");
    String msg;
    try
      {
        msg = "MUST throw NumberFormatException for out-of-range radix";
        try
          {
            new BigInteger("1234567890123", Character.MIN_RADIX - 1);
            harness.fail(msg);
          }
        catch (NumberFormatException x)
          {
            harness.check(true, msg);
          }
        try
          {
            new BigInteger("1234567890123", Character.MAX_RADIX + 1);
            harness.fail(msg);
          }
        catch (NumberFormatException x)
          {
            harness.check(true, msg);
          }

        msg = "MUST throw NumberFormatException for malformed string values";
        try
          {
            new BigInteger("  1234567890123", 32);
            harness.fail(msg);
          }
        catch (NumberFormatException x)
          {
            harness.check(true, msg);
          }
        try
          {
            new BigInteger("1234567890123  ", 32);
            harness.fail(msg);
          }
        catch (NumberFormatException x)
          {
            harness.check(true, msg);
          }
        try
          {
            new BigInteger("123456789-0123  ", 10);
            harness.fail(msg);
          }
        catch (NumberFormatException x)
          {
            harness.check(true, msg);
          }

        msg = "MUST throw NumberFormatException for invalid string values";
        try
          {
            new BigInteger("1ifbyland2ifbysea", 24);
            harness.fail(msg);
          }
        catch (NumberFormatException x)
          {
            harness.check(true, msg);
          }
      }
    catch (Exception x)
      {
        harness.verbose(x.getMessage());
        harness.fail("testCtorStringInt: " + x);
      }
  }

  private void testCtorIntIntRandom(TestHarness harness)
  {
    harness.checkPoint("testCtorIntIntRandom");
    Random prng = new Random();
    BigInteger bi;
    try
      {
        bi = new BigInteger(25, 80, prng);
        harness.check(bi.bitLength(), 25, "Constructed BigInteger MUST be 25-bit long");

        bi = new BigInteger(2, 80, prng);
        harness.check(bi.bitLength(), 2, "Constructed BigInteger MUST be 2-bit long");
        int val = bi.intValue();
        harness.check(val == 2 || val == 3, "Constructed BigInteger MUST be 2 or 3");
      }
    catch (Exception x)
      {
        harness.verbose(x.getMessage());
        harness.fail("testCtorIntIntRandom: " + x);
      }
  }

  private void testCtorIntRandom(TestHarness harness)
  {
    harness.checkPoint("testCtorIntRandom");
    Random prng = new Random();
    BigInteger bi;
    try
      {
        for (int realNumBits, numBits = 8; numBits < 2048; numBits++)
          {
            bi = new BigInteger(numBits, prng);
            realNumBits = bi.bitLength();
            if (realNumBits > numBits)
              harness.fail("Constructed BigInteger has " + realNumBits
                          + " bits when it SHOULD be <= " + numBits);
          }
      }
    catch (Exception x)
      {
        harness.verbose(x.getMessage());
        harness.fail("testCtorIntRandom: " + x);
      }
    harness.check(true, "BigInteger(numBits, prng) MUST generate MPIs shorter "
                  + "than numBits bits");
  }

  private void testCtorByteArray(TestHarness harness)
  {
    harness.checkPoint("testCtorByteArray");
    BigInteger b1, b2;
    byte[] ba;
    long val;
    try
      {
        for (int nValue = -0x8000; nValue < 0x8000; nValue += 8)
          {
            b1 = BigInteger.valueOf(nValue);
            ba = b1.toByteArray();
            b2 = new BigInteger(ba);
            val = b2.longValue();
            if (val != nValue)
              harness.fail("Re-constructed BigInteger long value was expected to be "
                           + nValue + " but was found to be " + val);
          }
      }
    catch (Exception x)
      {
        harness.verbose(x.getMessage());
        harness.fail("testCtorByteArray: " + x);
      }
    harness.check(true, "Re-constructed BigIntegers have expected long values");
  }
}
