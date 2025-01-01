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
public class addEdit
  implements Testlet
{
  public void test(TestHarness harness)
  {
    TestUndoManager mgr;

    // Check #1: Limit does not interfere.
    mgr = new TestUndoManager();
    mgr.setLimit(3);
    mgr.addTestEdits(10, 3);
    harness.check(mgr.checkIDs(new int[] { 10, 11, 12 }));

    // Check #2: Limit to two edits, but add three edits.
    mgr = new TestUndoManager();
    mgr.setLimit(2);
    mgr.addTestEdits(10, 3);
    harness.check(mgr.checkIDs(new int[] { 11, 12 }));

    // Check #3.
    mgr = new TestUndoManager();
    mgr.setLimit(3);
    mgr.addTestEdits(10, 4);
    mgr.undo();
    mgr.addTestEdits(100, 1);
    harness.check(mgr.checkIDs(new int[] { 11, 12, 100 }));

    // Check #4.
    mgr.undo();
    mgr.redo();
    mgr.undo();
    mgr.undo();
    mgr.addTestEdits(200, 1);
    harness.check(mgr.checkIDs(new int[] { 11, 200 }));

    // Check #5.
    mgr.undo();
    mgr.undo();
    mgr.addTestEdits(300, 1);
    harness.check(mgr.checkIDs(new int[] {300 }));
  }
}
