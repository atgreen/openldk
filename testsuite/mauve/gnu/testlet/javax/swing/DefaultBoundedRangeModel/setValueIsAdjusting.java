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
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.javax.swing.DefaultBoundedRangeModel;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import javax.swing.BoundedRangeModel;
import javax.swing.DefaultBoundedRangeModel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;


/**
 * Checks whether the DefaultBoundedRangeModel.setMaximum
 * method works correctly.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class setValueIsAdjusting
  implements Testlet
{
  public void test(TestHarness harness)
  {
    DefaultBoundedRangeModel dbrm;
    MyListener ml = new MyListener();

    // Check #1.
    dbrm = new DefaultBoundedRangeModel();
    dbrm.addChangeListener(ml);
    harness.check(!dbrm.getValueIsAdjusting());

    // Check #2.
    dbrm.setValueIsAdjusting(true);
    harness.check(dbrm.getValueIsAdjusting());

    // Check #3.
    dbrm.setMaximum(5);
    harness.check(ml.wasAdjusting);

    // Check #4.
    dbrm.setValueIsAdjusting(false);
    harness.check(!dbrm.getValueIsAdjusting());

    // Check #5.
    dbrm.setValue(2);
    harness.check(!ml.wasAdjusting);
  }


  static class MyListener
    implements ChangeListener
  {
    public boolean wasAdjusting = false;

    public void stateChanged(ChangeEvent evt)
    {
      wasAdjusting = ((BoundedRangeModel) evt.getSource())
        .getValueIsAdjusting();
    }
  }
}
