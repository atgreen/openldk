/* TestOfIntegerValueOf.java
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

package gnu.testlet.gnu.java.security.util;

import java.util.Random;

import gnu.java.security.util.IntegerUtil;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Speed test for the Integer RI 5 compatibility utility class.
 */
public class TestOfIntegerUtil
    implements Testlet
{
  private static final int INNER_ROUNDS = 1000;
  private static final int OUTER_ROUNDS = 2000;

  /* (non-Javadoc)
   * @see gnu.testlet.Testlet#test(gnu.testlet.TestHarness)
   */
  public void test(TestHarness harness)
  {
    int[] input = new int[100];
    int i, j;
    i = 0;
    for ( ; i < 7; i++)
      input[i] = i;
    j = 3;
    for ( ; i < 99; i++, j++)
      input[i] = 4 * j;

    Random rnd = new Random();
    int[] ndx = new int[INNER_ROUNDS];
    for (i = 0; i < INNER_ROUNDS; i++)
      ndx[i] = rnd.nextInt(100);

    String _100 = String.valueOf(100);
    long t1 = - System.currentTimeMillis();
    for (i = 0; i < OUTER_ROUNDS * INNER_ROUNDS; i++)
      Integer.valueOf(_100);
    t1 += System.currentTimeMillis();

    long t2 = - System.currentTimeMillis();
    for (i = 0; i < OUTER_ROUNDS * INNER_ROUNDS; i++)
      IntegerUtil.valueOf(_100);
    t2 += System.currentTimeMillis();

    harness.check(t2 <= t1,
                  "IntegerUtil.valueOf(String) MUST be faster than Integer.valueOf(String)");

    long t = - System.currentTimeMillis();
    for (i = 0; i < OUTER_ROUNDS; i++)
      for (j = 0; j < INNER_ROUNDS; j++)
        Integer.valueOf(String.valueOf(input[ndx[j]]));
    harness.verbose("*** Integer.valueOf(String) total time in ms. = "
                    + (t + System.currentTimeMillis()));

    t = - System.currentTimeMillis();
    for (i = 0; i < OUTER_ROUNDS; i++)
      for (j = 0; j < INNER_ROUNDS; j++)
        IntegerUtil.valueOf(String.valueOf(input[ndx[j]]));
    harness.verbose("*** IntegerUtil.valueOf(String) total time in ms. = "
                    + (t + System.currentTimeMillis()));

    t = - System.currentTimeMillis();
    for (i = 0; i < OUTER_ROUNDS; i++)
      for (j = 0; j < INNER_ROUNDS; j++)
        Integer.valueOf(input[ndx[j]]);
    harness.verbose("*** Integer.valueOf(int) total time in ms. = "
                    + (t + System.currentTimeMillis()));

    t = - System.currentTimeMillis();
    for (i = 0; i < OUTER_ROUNDS; i++)
      for (j = 0; j < INNER_ROUNDS; j++)
        IntegerUtil.valueOf(input[ndx[j]]);
    harness.verbose("*** IntegerUtil.valueOf(int) total time in ms. = "
                    + (t + System.currentTimeMillis()));
  }
}
