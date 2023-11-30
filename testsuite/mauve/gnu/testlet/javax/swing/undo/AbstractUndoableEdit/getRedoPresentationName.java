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

package gnu.testlet.javax.swing.undo.AbstractUndoableEdit;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import javax.swing.undo.AbstractUndoableEdit;
import javax.swing.UIManager;


/**
 * Checks whether the AbstractUndoableEdit.getRedoPresentationName method
 * works correctly.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class getRedoPresentationName
  implements Testlet
{
  public void test(TestHarness harness)
  {
    AbstractUndoableEdit edit;
    String oldValue;
    edit = new AbstractUndoableEdit()
    {
      public String getPresentationName()
      {
        return "Action";
      }
    };

    // Check #1.
    oldValue = UIManager.getString("AbstractUndoableEdit.undoText");
    UIManager.put("AbstractUndoableEdit.redoText", "RedoBar");
    harness.check(edit.getRedoPresentationName(), "RedoBar Action");

    // Check #2.
    edit = new AbstractUndoableEdit();
    harness.check(edit.getRedoPresentationName(), "RedoBar");

    UIManager.put("AbstractUndoableEdit.redoText", oldValue);
  }
}
