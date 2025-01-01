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
 * Some checks for the add() method in the {@link BigInteger} class.
 */
public class add implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)   
  {
    // a few simple cases
    BigInteger i1 = new BigInteger("-1");
    BigInteger i2 = new BigInteger("0");
    BigInteger i3 = new BigInteger("1");
    BigInteger i4 = new BigInteger("2");
    
    harness.check(i1.add(i2).equals(i1));
    harness.check(i1.add(i3).equals(i2));
    harness.check(i1.add(i4).equals(i3));
    harness.check(i3.add(i3).equals(i4));
    
    // some larger numbers
    BigInteger x = new BigInteger("123456789012345678901234567890");
    BigInteger y = new BigInteger("987654321098765432109876543210");
    harness.check(x.add(y).equals(new BigInteger("1111111110111111111011111111100")));
    
    // check a null argument
    boolean pass = false;
    try 
    {
      i1.add(null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
  }

}
