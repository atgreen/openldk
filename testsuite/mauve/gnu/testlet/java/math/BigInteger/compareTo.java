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
 * Some checks for the compareTo() method in the {@link BigInteger} class.
 */
public class compareTo implements Testlet 
{
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)   
  {
    BigInteger a = new BigInteger("-987654321098765432109876543210");
    BigInteger b = new BigInteger("-1");
    BigInteger c = new BigInteger("0");
    BigInteger d = new BigInteger("1");
    BigInteger e = new BigInteger("987654321098765432109876543210");
    
    harness.check(a.compareTo(a) == 0);
    harness.check(a.compareTo(b) < 0);
    harness.check(a.compareTo(c) < 0);
    harness.check(a.compareTo(d) < 0);
    harness.check(a.compareTo(e) < 0);

    harness.check(b.compareTo(a) > 0);
    harness.check(b.compareTo(b) == 0);
    harness.check(b.compareTo(c) < 0);
    harness.check(b.compareTo(d) < 0);
    harness.check(b.compareTo(e) < 0);

    harness.check(c.compareTo(a) > 0);
    harness.check(c.compareTo(b) > 0);
    harness.check(c.compareTo(c) == 0);
    harness.check(c.compareTo(d) < 0);
    harness.check(c.compareTo(e) < 0);

    harness.check(d.compareTo(a) > 0);
    harness.check(d.compareTo(b) > 0);
    harness.check(d.compareTo(c) > 0);
    harness.check(d.compareTo(d) == 0);
    harness.check(d.compareTo(e) < 0);

    harness.check(e.compareTo(a) > 0);
    harness.check(e.compareTo(b) > 0);
    harness.check(e.compareTo(c) > 0);
    harness.check(e.compareTo(d) > 0);
    harness.check(e.compareTo(e) == 0);
    
    // try the compareTo(Object) method
    boolean pass = false;
    try
    {
      ((Comparable)a).compareTo(new Integer(1));
    }
    catch (ClassCastException ee) 
    {
      pass = true;
    }
    harness.check(pass);
    
    // try a null argument
    pass = false;
    try 
    {
      a.compareTo(null);
    }
    catch (NullPointerException ee) 
    {
      pass = true;
    }
    harness.check(pass);
  }

}
