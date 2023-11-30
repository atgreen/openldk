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
 * Checks whether the DefaultBoundedRangeModel.setValue
 * method works correctly.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class setValue
  implements Testlet
{
  public void test(TestHarness harness)
  {
    DefaultBoundedRangeModel dbrm = new DefaultBoundedRangeModel();

    // Check #1: Value between min and max.
    dbrm.setRangeProperties(1, 1, 0, 10, false);
    dbrm.setValue(5);
    setRangeProperties.check(harness, dbrm, 5, 1, 0, 10, false);

    // Check #2: Value < min.
    dbrm.setRangeProperties(4, 1, 3, 10, false);
    dbrm.setValue(-5);
    setRangeProperties.check(harness, dbrm, 3, 1, 3, 10, false);

    // Check #3: Value > max.
    dbrm.setRangeProperties(4, 0, 3, 10, false);
    dbrm.setValue(140);
    setRangeProperties.check(harness, dbrm, 10, 0, 3, 10, false);

    // Check #4: Value + extent > max.
    dbrm.setRangeProperties(4, 4, 3, 10, false);
    dbrm.setValue(9);
    setRangeProperties.check(harness, dbrm, 6, 4, 3, 10, false);
  }
}
