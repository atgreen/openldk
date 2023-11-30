// Tags: JDK1.2
// Uses: setRangeProperties

// Copyright (C) 2003 Sascha Brawer <brawer@dandelis.ch>

// This file is part of Mauve.

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

package gnu.testlet.javax.swing.DefaultBoundedRangeModel;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import javax.swing.DefaultBoundedRangeModel;

/**
 * Checks whether the DefaultBoundedRangeModel.setMaximum
 * method works correctly.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class setMaximum
  implements Testlet
{
  public void test(TestHarness harness)
  {
    DefaultBoundedRangeModel dbrm = new DefaultBoundedRangeModel();

    // Check #1: Increase maximum.
    dbrm.setRangeProperties(1, 1, 0, 10, false);
    dbrm.setMaximum(20);
    setRangeProperties.check(harness, dbrm, 1, 1, 0, 20, false);

    // Check #2: Decrease maximum.
    dbrm.setRangeProperties(600, 10, -3, 700, true);
    dbrm.setMaximum(543);
    setRangeProperties.check(harness, dbrm, 533, 10, -3, 543, true);

    // Check #3: Decrease maximum below minimum.
    dbrm.setRangeProperties(600, 10, -3, 700, true);
    dbrm.setMaximum(-4);
    setRangeProperties.check(harness, dbrm, -4, 0, -4, -4, true);
  }
}
