// Tags: JDK1.4

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

package gnu.testlet.java.awt.BasicStroke;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.BasicStroke;

/**
 * Some checks for the equals() method in the {@link BasicStroke} class.
 */
public class equals implements Testlet {

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    BasicStroke s1 = new BasicStroke();
    BasicStroke s2 = new BasicStroke();
    harness.check(s1.equals(s2));
    
    s1 = new BasicStroke(2.0f);
    harness.check(!s1.equals(s2));
    s2 = new BasicStroke(2.0f);
    harness.check(s1.equals(s2));
    
    s1 = new BasicStroke(3.0f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER);
    harness.check(!s1.equals(s2));
    s2 = new BasicStroke(3.0f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER);
    harness.check(s1.equals(s2));

    s1 = new BasicStroke(3.0f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_ROUND);
    harness.check(!s1.equals(s2));
    s2 = new BasicStroke(3.0f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_ROUND);
    harness.check(s1.equals(s2));

    s1 = new BasicStroke(3.0f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_ROUND, 15.0f);
    harness.check(!s1.equals(s2));
    s2 = new BasicStroke(3.0f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_ROUND, 15.0f);
    harness.check(s1.equals(s2));

    s1 = new BasicStroke(3.0f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_ROUND, 15.0f, new float[] {1.0f, 2.0f}, 0.0f);
    harness.check(!s1.equals(s2));
    s2 = new BasicStroke(3.0f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_ROUND, 15.0f, new float[] {1.0f, 2.0f}, 0.0f);
    harness.check(s1.equals(s2));

    s1 = new BasicStroke(3.0f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_ROUND, 15.0f, new float[] {1.0f, 2.0f}, 1.0f);
    harness.check(!s1.equals(s2));
    s2 = new BasicStroke(3.0f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_ROUND, 15.0f, new float[] {1.0f, 2.0f}, 1.0f);
    harness.check(s1.equals(s2));
  }

}
