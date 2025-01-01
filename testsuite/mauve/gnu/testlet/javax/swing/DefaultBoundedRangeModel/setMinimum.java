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
 * Checks whether the DefaultBoundedRangeModel.setMinimum
 * method works correctly.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class setMinimum
  implements Testlet
{
  public void test(TestHarness harness)
  {
    DefaultBoundedRangeModel dbrm = new DefaultBoundedRangeModel();

    // Check #1: Increase miminum. Value must not change.
    dbrm.setRangeProperties(7, 2, 0, 10, true);
    dbrm.setMinimum(3);
    setRangeProperties.check(harness, dbrm, 7, 2, 3, 10, true);

    // Check #2: Increase miminum. Value must be changed.
    dbrm.setRangeProperties(7, 2, 0, 10, false);
    dbrm.setMinimum(8);
    setRangeProperties.check(harness, dbrm, 8, 2, 8, 10, false);

    // Check #3: Increase miminum. Value and extent must be changed.
    dbrm.setRangeProperties(7, 2, 0, 10, false);
    dbrm.setMinimum(9);
    setRangeProperties.check(harness, dbrm, 9, 1, 9, 10, false);

    // Check #4: Increase miminum beyond max.
    dbrm.setRangeProperties(7, 2, 0, 10, false);
    dbrm.setMinimum(200);
    setRangeProperties.check(harness, dbrm, 200, 0, 200, 200, false);

    // Check #5: Decrease minimum. Value must not change.
    dbrm.setRangeProperties(7, 2, 0, 10, false);
    dbrm.setMinimum(-20);
    setRangeProperties.check(harness, dbrm, 7, 2, -20, 10, false);
  }
}
