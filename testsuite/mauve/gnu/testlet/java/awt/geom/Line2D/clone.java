//Tags: JDK1.2

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

package gnu.testlet.java.awt.geom.Line2D;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.geom.Line2D;

/**
* Checks that Line2D.clone() method works correctly.
*/
public class clone implements Testlet {

    /**
     * Runs the test using the specified harness.
     * 
     * @param harness  the test harness (<code>null</code> not permitted).
     */
    public void test(TestHarness harness) {
        Line2D line1 = new Line2D.Double(1.0, 2.0, 3.0, 4.0);
        Line2D line2 = null;
        line2 = (Line2D) line1.clone();
        harness.check(line1.getX1() == line2.getX1());
        harness.check(line1.getX2() == line2.getX2());
        harness.check(line1.getY1() == line2.getY1());
        harness.check(line1.getY2() == line2.getY2());
        harness.check(line1.getClass() == line2.getClass());
        harness.check(line1 != line2);
    }

}
