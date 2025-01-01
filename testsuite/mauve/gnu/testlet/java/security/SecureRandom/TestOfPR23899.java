/* TestOfPR23899.java
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

package gnu.testlet.java.security.SecureRandom;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.security.SecureRandom;

/**
 * Regression test for PR Classpath/23899
 */
public class TestOfPR23899
    implements Testlet
{
  public void test(TestHarness harness)
  {
    harness.checkPoint("TestOfPR23899");
    SecureRandom prng1, prng2;
    int ra, rb;
    try
      {
        prng1 = SecureRandom.getInstance("SHA1PRNG");
        prng2 = SecureRandom.getInstance("SHA1PRNG");
        ra = prng1.nextInt();
        rb = prng2.nextInt();
        harness.check(ra != rb,
                      "Similar SecureRandoms MUST NOT generate same bytes when "
                      + "not explicitly seeded");

        prng1 = SecureRandom.getInstance("SHA1PRNG");
        prng1.setSeed(98243647L);
        prng2 = SecureRandom.getInstance("SHA1PRNG");
        prng2.setSeed(98243647L);
        ra = prng1.nextInt();
        rb = prng2.nextInt();
        harness.check(ra == rb,
                      "Similar SecureRandoms MUST generate same bytes when "
                      + "similarly seeded");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfPR23899: " + x);
      }
  }
}
