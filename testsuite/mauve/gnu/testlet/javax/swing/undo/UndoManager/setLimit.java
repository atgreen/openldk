// Tags: JDK1.2
// Uses: TestUndoManager

// Copyright (C) 2004 Sascha Brawer <brawer@dandelis.ch>

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

package gnu.testlet.javax.swing.undo.UndoManager;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;


/**
 * @author <a href="mailto:brawer@dandelis.ch">Sascha Brawer</a>
 */
public class setLimit
  implements Testlet
{
  public void test(TestHarness harness)
  {
    TestUndoManager mgr = new TestUndoManager();
    Throwable caught;

    // Check #1.
    mgr.setLimit(25);
    harness.check(mgr.getLimit(), 25);
    
    // Check #2.
    // Sun JDK 1.4.1_01 accepts zero values, although this
    // does not really make much sense.
    mgr.setLimit(0);
    harness.check(mgr.getLimit(), 0);

    // Check #3.
    // Sun JDK 1.4.1_01 accepts negative values, although this
    // does not really make any sense.
    mgr.setLimit(-10);
    harness.check(mgr.getLimit(), -10);

    // Check #4.
    harness.check(mgr.numTrimForLimitCalls, 3);

    // Check #5.
    mgr.setLimit(20);
    mgr.addTestEdits(10, 5);
    harness.check(mgr.checkIDs(new int[] { 10, 11, 12, 13, 14 }));

    // Check #6.
    mgr.undo();
    mgr.undo();
    mgr.setLimit(2);
    harness.check(mgr.checkIDs(new int[] { 12, 13 }));

    // Check #7.
    mgr.end();
    caught = null;
    try
      {
        mgr.setLimit(2);
      }
    catch (Exception ex)
      {
        caught = ex;
      }
    harness.check(caught != null);
  }
}
