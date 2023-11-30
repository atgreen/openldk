//Tags: JDK1.1

//Copyright (C) 2004 David Gilbert <david.gilbert@object-refinery.com>

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

package gnu.testlet.java.awt.Dimension;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Dimension;

/**
 * Checks that the setSize() method in the {@link Dimension} class works 
 * correctly.  See Sun's bug parade reports 4245442 and 4976448.  This 
 * is still a problem in JDK 1.3.1_11.
 */
public class setSize implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)  
  {
    Dimension d = new Dimension();
    d.setSize(1, 2);
    harness.check(d.getWidth() == 1);
    harness.check(d.getHeight() == 2);
   
    d.setSize(5.0, 10.0);
    harness.check(d.getWidth() == 5.0);
    harness.check(d.getHeight() == 10.0);

    double w = Integer.MAX_VALUE + 100000.0;
    double h = Integer.MAX_VALUE + 200000.0;
    d.setSize(w, h);
    harness.check(d.getWidth() == Integer.MAX_VALUE);
    harness.check(d.getHeight() == Integer.MAX_VALUE);

    // check for null argument
    boolean pass = false;
    try 
    {
      d.setSize(null);
    }
    catch (NullPointerException e) 
    {
      pass = true;
    }
    harness.check(pass);
  }

}
