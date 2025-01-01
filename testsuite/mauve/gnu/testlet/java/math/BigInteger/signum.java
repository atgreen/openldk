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
 * Some checks for the signum() method in the {@link BigInteger} class.
*/
public class signum implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)   
  {
    BigInteger a = new BigInteger("-123456789012345678901234567890");
    BigInteger b = new BigInteger("-1");
    BigInteger c = new BigInteger("0");
    BigInteger d = new BigInteger("1");
    BigInteger e = new BigInteger("123456789012345678901234567890");

    harness.check(a.signum() == -1);
    harness.check(b.signum() == -1);
    harness.check(c.signum() == 0);
    harness.check(d.signum() == 1);
    harness.check(e.signum() == 1);

  }

}
