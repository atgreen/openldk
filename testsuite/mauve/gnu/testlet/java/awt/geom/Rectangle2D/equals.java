//Tags: JDK1.2

//Copyright (C) 2004 David Gilbert (david.gilbert@object-refinery.com)

//This file is part of Mauve.

//Mauve is free software; you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation; either version 2, or (at your option)
//any later version.

//Mauve is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.

//You should have received a copy of the GNU General Public License
//along with Mauve; see the file COPYING.  If not, write to
//the Free Software Foundation, 59 Temple Place - Suite 330,
//Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.awt.geom.Rectangle2D;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.geom.Rectangle2D;

/**
 * Tests the equals() method for the {@link Rectangle2D} class.
 */
public class equals
  implements Testlet
{
  /**
   * Some checks for the equals() method.
   */
  public void test(TestHarness harness) 
  {
    Rectangle2D r1 = new Rectangle2D.Double();
    Rectangle2D r2 = new Rectangle2D.Double();
    harness.check(r1.equals(r2));
    harness.check(r2.equals(r1));
    harness.check(!r1.equals(null));
    
    r1 = new Rectangle2D.Double(1.0, 0.0, 0.0, 0.0);
    harness.check(!r1.equals(r2));
    r2 = new Rectangle2D.Double(1.0, 0.0, 0.0, 0.0);
    harness.check(r1.equals(r2));

    r1 = new Rectangle2D.Double(1.0, 2.0, 0.0, 0.0);
    harness.check(!r1.equals(r2));
    r2 = new Rectangle2D.Double(1.0, 2.0, 0.0, 0.0);
    harness.check(r1.equals(r2));

    r1 = new Rectangle2D.Double(1.0, 2.0, 3.0, 0.0);
    harness.check(!r1.equals(r2));
    r2 = new Rectangle2D.Double(1.0, 2.0, 3.0, 0.0);
    harness.check(r1.equals(r2));

    r1 = new Rectangle2D.Double(1.0, 2.0, 3.0, 4.0);
    harness.check(!r1.equals(r2));
    r2 = new Rectangle2D.Double(1.0, 2.0, 3.0, 4.0);
    harness.check(r1.equals(r2));

  }

}