// Tags: JDK1.2

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
// Boston, MA 02111-1307, USA.

package gnu.testlet.javax.swing.DefaultBoundedRangeModel;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import javax.swing.DefaultBoundedRangeModel;

/**
 * Checks whether the DefaultBoundedRangeModel.setRangeProperties
 * method works correctly.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class setRangeProperties
  implements Testlet
{
  public void test(TestHarness harness)
  {
    // Check #1: Normal parameters
    DefaultBoundedRangeModel dbrm = new DefaultBoundedRangeModel();
    dbrm.setRangeProperties(5, 2, 1, 8, false);
    check(harness, dbrm, 5, 2, 1, 8, false);

    // Check #2: valueIsAdjusting
    dbrm.setRangeProperties(5, 2, 1, 8, true);
    check(harness, dbrm, 5, 2, 1, 8, true);

    // Check #3: extent < 0
    dbrm.setRangeProperties(4, -2, -4, 9, false);
    check(harness, dbrm, 4, 0, -4, 9, false);

    // Check #4: value > maximum
    dbrm.setRangeProperties(14, 0, 1, 8, false);
    check(harness, dbrm, 14, 0, 1, 14, false);

    // Check #5: value + extent > maximum
    dbrm.setRangeProperties(5, 4, 1, 8, false);
    check(harness, dbrm, 5, 3, 1, 8, false);

    // Check #6: value < minimum
    dbrm.setRangeProperties(-3, 1, 0, 8, false);
    check(harness, dbrm, -3, 1, -3, 8, false);
  }


  public static void check(TestHarness harness,
                            DefaultBoundedRangeModel brm,
                            int value, int extent,
                            int minimum, int maximum,
                            boolean adjusting)
  {
    int a;
    boolean b;

    if ((a = brm.getValue()) != value)
    {
      harness.check(false);
      harness.debug("got value " + a + " but expected " + value);
      return;
    }

    if ((a = brm.getExtent()) != extent)
    {
      harness.check(false);
      harness.debug("got extent " + a + " but expected " + extent);
      return;
    }

    if ((a = brm.getMinimum()) != minimum)
    {
      harness.check(false);
      harness.debug("got minimum " + a + " but expected " + minimum);
      return;
    }

    if ((a = brm.getMaximum()) != maximum)
    {
      harness.check(false);
      harness.debug("got maximum " + a + " but expected " + maximum);
      return;
    }

    if ((b = brm.getValueIsAdjusting()) != adjusting)
    {
      harness.check(false);
      harness.debug("got adjusting " + b + " but expected " + adjusting);
      return;
    }

    harness.check(true);
  }
}
