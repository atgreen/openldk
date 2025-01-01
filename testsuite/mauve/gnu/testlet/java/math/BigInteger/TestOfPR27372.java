/* TestOfPR27372.java
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
import java.security.SecureRandom;
import java.util.Arrays;

/**
 * Regression test for PR Classpath/27372
 */
public class TestOfPR27372
    implements Testlet
{
  /** A control value. */
  private static final byte[] BYTES = { 0x12, 0x34, 0x56, 0x78 };
  /** Set to <code>true</code> if BigInteger is using the GNU MP native code. */
  private boolean usingNativeImpl = Arrays.equals(BYTES,
                                                  new BigInteger(BYTES).toByteArray());

  public void test(TestHarness harness)
  {
    harness.checkPoint("TestOfPR27372");
    SecureRandom prng1, prng2;
    byte[] ba, bb;
    try
      {
        prng1 = SecureRandom.getInstance("SHA1PRNG");
        prng1.setSeed(98243647L);
        prng2 = SecureRandom.getInstance("SHA1PRNG");
        prng2.setSeed(98243647L);
        // from here on the two PRNGs are in sync.  they should consume as
        // little bytes as possible, and continue to be in sync
        for (int numBytes = 1; numBytes < 10; numBytes++)
          {
            ba = new BigInteger(8 * numBytes, prng1).toByteArray();
            bb = new byte[numBytes];
            prng2.nextBytes(bb);
            harness.check(areEqual(ba, bb),
                          "BigInteger(int, Random) SHOULD consume as little "
                          + "bytes as possible from Random (and SecureRandom): "
                          + numBytes);
          }
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfPR27372: " + x);
      }
  }

  /**
   * In both cases --the pure Java implementation, and the native one based on
   * the GNU MP library-- a BigInteger's toByteArray(), can produce an extra
   * 0x00 byte as the most significant byte.  This method ensures that there
   * is no more than just one zero-byte at the high end, and then channels the
   * call to the appropriate are-equal method depending on the type of the
   * underlying implementation of the MPI.
   * 
   * @param a the result of a {@link BigInteger#toByteArray()} of an instance
   *          constructed with {@link BigInteger#BigInteger(int, Random)}.
   * @param b a non-null byte array filled with randomly generated values.
   * @return <code>true</code> if the two byte arrays contain the same values
   *         taking into consideration how our BigInteger constructs its
   *         internal data structures.
   */
  private boolean areEqual(byte[] a, byte[] b)
  {
    int offset = 0;
    switch (a.length - b.length)
    {
      case 0:
        break;
      case 1:
        if (a[0] != 0)
          return false;
        offset = 1;
        break;
      default:
        return false;
    }
    if (usingNativeImpl)
      return areEqualNativeBI(a, offset, b);
    return areEqualJavaBI(a, offset, b);
  }

  /**
   * Special byte array comparison method used to compare the result of
   * <code>byte[] a = new BigInteger(numBits, random).toByteArray()</code> with
   * a byte array filled with randomly generated values.
   * <p>
   * This method takes into consideration how an array of bytes is used to
   * fill the (pure java) <code>BigInteger</code>'s internal data structure. As
   * an example, here is what the two byte arrays may look like from the
   * outside:
   * 
   * <pre>
   *   a = 009ECB38BFD4C6
   *   b = CB9EC6D4BF38
   * </pre>
   * 
   * @param a the result of a {@link BigInteger#toByteArray()} of an instance
   *          constructed with {@link BigInteger#BigInteger(int, Random)}.
   * @param <code>0</code> or <code>1</code> depending on the value of the
   *          leftmost byte (at index #0) of <code>a</code>.
   * @param b a non-null byte array filled with randomly generated values.
   * @return <code>true</code> if the two byte arrays contain the same values
   *         taking into consideration how our BigInteger constructs its
   *         internal data structures.
   */
  private boolean areEqualJavaBI(byte[] a, int offset, byte[] b)
  {
    for (int i = b.length - 1, j, k; i >= 0; )
      {
        j = i - 3;
        if (j < 0)
          j = 0;
        for (k = 0; k < 4; k++)
          {
            if (a[offset + j++] != b[i--])
              return false;
            if (i < 0)
              break;
          }
      }
    return true;
  }

  /**
   * Straight equality check, byte-for-byte, of the two designated arrays. The
   * first starting from either <code>0</code> or <code>1</code>, while the
   * second always starting from <code>0</code>. This is used with GMP-based
   * MPIs.
   * 
   * @param a the result of a {@link BigInteger#toByteArray()} of an instance
   *          constructed with {@link BigInteger#BigInteger(int, Random)}.
   * @param <code>0</code> or <code>1</code> depending on the value of the
   *          leftmost byte (at index #0) of <code>a</code>.
   * @param b a non-null byte array filled with randomly generated values.
   * @return <code>true</code> if the two byte arrays contain the same values
   *         taking into consideration how our BigInteger constructs its
   *         internal data structures.
   */
  private boolean areEqualNativeBI(byte[] a, int offset, byte[] b)
  {
    for (int i = 0; i < b.length; i++)
      if (a[offset + i] != b[i])
        return false;
    return true;
    
  }
}
