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


/**
 * Checks whether the AbstractUndoableEdit.canUndo method
 * works correctly.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class canUndo
  implements Testlet
{
  public void test(TestHarness harness)
  {
    AbstractUndoableEdit edit;

    edit = new AbstractUndoableEdit();

    // Check #1.
    harness.check(edit.canUndo());   

    // Check #2.
    edit.undo();
    harness.check(!edit.canUndo());

    // Check #3.
    edit.redo();
    harness.check(edit.canUndo());

    // Check #4.
    edit.die();
    harness.check(!edit.canUndo());
  }
}
