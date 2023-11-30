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
// Boston, MA 02111-1307, USA.

package gnu.testlet.javax.swing.DefaultBoundedRangeModel;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

/**
 * Checks whether the DefaultBoundedRangeModel.toString
 * method works correctly.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class general
  implements Testlet
{
  public void test(TestHarness harness)
  {
    javax.swing.DefaultBoundedRangeModel dbrm;
    Throwable caught;

    // Check #1.
    dbrm = new javax.swing.DefaultBoundedRangeModel();
    setRangeProperties.check(harness, dbrm, 0, 0, 0, 100, false);

    // Check #2.
    dbrm = new javax.swing.DefaultBoundedRangeModel(5, 2, -1234, 4321);
    setRangeProperties.check(harness, dbrm, 5, 2, -1234, 4321, false);

    // Check #3.
    caught = null;
    try
      {
        dbrm = new javax.swing.DefaultBoundedRangeModel(-2, 0, 10, 20);
      }
    catch (Exception ex)
      {
        caught = ex;
      }
    harness.check(caught instanceof IllegalArgumentException);
  }
}
