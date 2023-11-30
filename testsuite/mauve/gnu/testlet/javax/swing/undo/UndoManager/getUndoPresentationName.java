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
import javax.swing.UIManager;

/**
 * @author <a href="mailto:brawer@dandelis.ch">Sascha Brawer</a>
 */
public class getUndoPresentationName
  implements Testlet
{
  public void test(TestHarness harness)
  {
    TestUndoManager mgr;
    TestUndoManager.TestEdit[] edits;
    Object oldText;

    mgr = new TestUndoManager();
    edits = mgr.addTestEdits(1, 2);

    // Check #1.
    harness.check(mgr.getUndoPresentationName(), "UndoPres2");

    // Check #2.
    mgr.undo();
    harness.check(mgr.getUndoPresentationName(), "UndoPres1");

    // Check #3.
    mgr.undo();
    oldText = UIManager.get("AbstractUndoableEdit.undoText");
    try
      {
        UIManager.put("AbstractUndoableEdit.undoText", "Foondo");
        harness.check(mgr.getUndoPresentationName(), "Foondo");
      }
    finally
      {
        UIManager.put("AbstractUndoableEdit.undoText", oldText);
      }

    // Check #4.
    mgr.redo();
    harness.check(mgr.getUndoPresentationName(), "UndoPres1");

    // Check #5.
    mgr.end();
    harness.check(mgr.getUndoPresentationName(), "UndoPres1");
  }
}
