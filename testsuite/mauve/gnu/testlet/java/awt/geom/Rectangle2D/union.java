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
 * Some tests for the union() method in the {@link Rectangle2D} class.
 */
public class union
  implements Testlet
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)
  {
    Rectangle2D r1 = new Rectangle2D.Double(1.0, 2.0, 3.0, 4.0);
    Rectangle2D r2 = new Rectangle2D.Double(5.0, 6.0, 7.0, 8.0);
    Rectangle2D r3 = new Rectangle2D.Double();
    Rectangle2D.union(r1, r2, r3);
    harness.check(r3.getX(), 1.0);
    harness.check(r3.getY(), 2.0);
    harness.check(r3.getWidth(), 11.0);
    harness.check(r3.getHeight(), 12.0);
    
    boolean pass = false;
    try 
    {
      Rectangle2D.union(null, r2, r3);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    pass = false;
    try 
    {
      Rectangle2D.union(r1, null, r3);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    pass = false;
    try 
    {
      Rectangle2D.union(r1, r2, null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
  }

}