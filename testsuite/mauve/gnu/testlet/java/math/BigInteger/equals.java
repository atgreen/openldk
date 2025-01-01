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
 * Some checks for the equals() method in the {@link BigInteger} class.
 */
public class equals implements Testlet 
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
 
    BigInteger aa = new BigInteger("-987654321098765432109876543210");
    BigInteger bb = new BigInteger("-1");
    BigInteger cc = new BigInteger("0");
    BigInteger dd = new BigInteger("1");
    BigInteger ee = new BigInteger("987654321098765432109876543210");

    harness.check(a.equals(aa));
    harness.check(!a.equals(bb));
    harness.check(!a.equals(cc));
    harness.check(!a.equals(dd));
    harness.check(!a.equals(ee));
    harness.check(!a.equals(null));

    harness.check(!b.equals(aa));
    harness.check(b.equals(bb));
    harness.check(!b.equals(cc));
    harness.check(!b.equals(dd));
    harness.check(!b.equals(ee));
    harness.check(!b.equals(null));
    harness.check(!b.equals(new Integer(-1)));

    harness.check(!c.equals(aa));
    harness.check(!c.equals(bb));
    harness.check(c.equals(cc));
    harness.check(!c.equals(dd));
    harness.check(!c.equals(ee));
    harness.check(!c.equals(null));
    harness.check(!c.equals(new Integer(0)));

    harness.check(!d.equals(aa));
    harness.check(!d.equals(bb));
    harness.check(!d.equals(cc));
    harness.check(d.equals(dd));
    harness.check(!d.equals(ee));
    harness.check(!d.equals(null));
    harness.check(!d.equals(new Integer(1)));

    harness.check(!e.equals(aa));
    harness.check(!e.equals(bb));
    harness.check(!e.equals(cc));
    harness.check(!e.equals(dd));
    harness.check(e.equals(ee));
    harness.check(!e.equals(null));

 
  }

}
