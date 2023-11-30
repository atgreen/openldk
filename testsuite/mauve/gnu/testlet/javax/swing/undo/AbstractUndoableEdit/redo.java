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
import javax.swing.undo.CannotRedoException;


/**
 * Checks whether the AbstractUndoableEdit.redo method
 * works correctly.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class redo
  implements Testlet
{
  private class MyEdit
    extends AbstractUndoableEdit
  {
    public boolean redoable;

    public boolean canRedo()
    {
      return redoable;
    }
  }


  public void test(TestHarness harness)
  {
    MyEdit edit;
    Throwable caught;

    // Check #1: No exception for redoable = true.
    edit = new MyEdit();
    edit.redoable = true;
    edit.undo();
    edit.redo();
    harness.check(true);

    // Check #2: Exception for redoable = false;
    edit.redoable = false;
    try
      {
        edit.undo();
        edit.redo();
        caught = null;
      }
    catch (Exception ex)
      {
        caught = ex;
      }
    harness.check(caught instanceof CannotRedoException);

    // Check #3: Exception for dead edit.
    AbstractUndoableEdit aue = new AbstractUndoableEdit();
    aue.undo();
    aue.die();
    try
      {
        aue.redo();
        caught = null;
      }
    catch (Exception ex)
      {
        caught = ex;
      }
    harness.check(caught instanceof CannotRedoException);
  }
}
