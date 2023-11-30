// Tags: JDK1.1

// Copyright (C) 2004 David Gilbert <david.gilbert@object-refinery.com>

// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.

// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.math.BigInteger;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.math.BigInteger;

/**
 * Some checks for the valueOf() method in the {@link BigInteger} class.
 */
public class valueOf implements Testlet 
{
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)   
  {
    // can't think of any obvious candidates to test, but...
    harness.check(BigInteger.valueOf(0).equals(new BigInteger("0")));
    harness.check(BigInteger.valueOf(1).equals(new BigInteger("1")));
    harness.check(BigInteger.valueOf(-1).equals(new BigInteger("-1")));
    harness.check(BigInteger.valueOf(Long.MAX_VALUE).equals(new BigInteger("9223372036854775807")));
    harness.check(BigInteger.valueOf(Long.MIN_VALUE).equals(new BigInteger("-9223372036854775808")));    
  }

}
