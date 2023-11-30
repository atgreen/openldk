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
 * Some checks for the multiply() method in the {@link BigInteger} class.
 */
public class multiply implements Testlet 
{
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)   
  {
    // some really simple cases
    BigInteger p1 = new BigInteger("1");
    BigInteger p2 = new BigInteger("2");
    BigInteger m1 = new BigInteger("-1");
    BigInteger m2 = new BigInteger("-2");
 
    harness.check(p1.multiply(p2).equals(p2));
    harness.check(p1.multiply(m2).equals(m2));
    harness.check(m1.multiply(p2).equals(m2));
    harness.check(m1.multiply(m2).equals(p2));

    // some bigger numbers
    BigInteger bp1 = new BigInteger("12345678901234567890123456789012345678901234567890");
    BigInteger bp2 = new BigInteger("987654321098765432198765");
    BigInteger bm1 = new BigInteger("-12345678901234567890123456789012345678901234567890");
    BigInteger bm2 = new BigInteger("-987654321098765432198765");
    BigInteger resultp = new BigInteger("12193263113702179523715891618930089161893008916189178958987793067366655850");
    BigInteger resultm = new BigInteger("-12193263113702179523715891618930089161893008916189178958987793067366655850");
 
    harness.check(bp1.multiply(bp2).equals(resultp));
    harness.check(bp1.multiply(bm2).equals(resultm));
    harness.check(bm1.multiply(bp2).equals(resultm));
    harness.check(bm1.multiply(bm2).equals(resultp));

    // check null argument
    boolean pass = false;
    try 
    {
      p1.multiply(null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
  }

}
