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

import javax.swing.undo.AbstractUndoableEdit;

/**
 * @author <a href="mailto:brawer@dandelis.ch">Sascha Brawer</a>
 */
public class trimEdits
  implements Testlet
{
  public void test(TestHarness harness)
  {
    TestUndoManager mgr = new TestUndoManager();
    TestEdit[] e;

    e = new TestEdit[6];
    for (int i = 0; i < e.length; i++)
    {
      e[i] = new TestEdit();
      mgr.addEdit(e[i]);
    }

    harness.check(mgr.getEdits().size(), 6);        // Check #1.
    mgr.invokeTrimEdits(2, 3);
    harness.check(mgr.getEdits().size(), 4);        // Check #2.
    harness.check(mgr.getEdits().get(0) == e[0]);   // Check #3.
    harness.check(mgr.getEdits().get(1) == e[1]);   // Check #4.
    harness.check(mgr.getEdits().get(2) == e[4]);   // Check #5.
    harness.check(mgr.getEdits().get(3) == e[5]);   // Check #6.
    harness.check(e[0].isAlive());                  // Check #7.
    harness.check(e[1].isAlive());                  // Check #8.
    harness.check(!e[2].isAlive());                 // Check #9.
    harness.check(!e[3].isAlive());                 // Check #10.
    harness.check(e[4].isAlive());                  // Check #11.
    harness.check(e[5].isAlive());                  // Check #12.
    harness.check(mgr.getEditToBeUndone() == e[5]); // Check #13.

    // Check #14 .. #24: from == to.
    mgr.invokeTrimEdits(0, 0);
    harness.check(mgr.getEdits().size(), 3);        // Check #14.
    harness.check(mgr.getEdits().get(0) == e[1]);   // Check #15.
    harness.check(mgr.getEdits().get(1) == e[4]);   // Check #16.
    harness.check(mgr.getEdits().get(2) == e[5]);   // Check #17.
    harness.check(!e[0].isAlive());                 // Check #18.
    harness.check(e[1].isAlive());                  // Check #19.
    harness.check(!e[2].isAlive());                 // Check #20.
    harness.check(!e[3].isAlive());                 // Check #21.
    harness.check(e[4].isAlive());                  // Check #22.
    harness.check(e[5].isAlive());                  // Check #23.
    harness.check(mgr.getEditToBeUndone() == e[5]); // Check #24.

    // Check #25 .. #29: Nothing happens if from > to.
    mgr.invokeTrimEdits(1, 0);
    mgr.invokeTrimEdits(2222, -100);
    harness.check(mgr.getEdits().size(), 3);        // Check #25.
    harness.check(mgr.getEdits().get(0) == e[1]);   // Check #26.
    harness.check(mgr.getEdits().get(1) == e[4]);   // Check #27.
    harness.check(mgr.getEdits().get(2) == e[5]);   // Check #28.
    harness.check(mgr.getEditToBeUndone() == e[5]); // Check #29.

    mgr.undo();
    mgr.invokeTrimEdits(0, 1);
    harness.check(mgr.getEdits().size(), 1);        // Check #30.
    harness.check(mgr.getEdits().get(0) == e[5]);   // Check #31.
    harness.check(mgr.getEditToBeUndone() == null); // Check #32.
  }


  private static class TestEdit
    extends AbstractUndoableEdit
  {
    public boolean isAlive()
    {
      return canUndo() || canRedo();
    }
  }
}
