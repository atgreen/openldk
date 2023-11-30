// Tags: JDK1.2

// Copyright (C) 2003, 2004 Sascha Brawer <brawer@dandelis.ch>

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

package gnu.testlet.javax.swing.undo.CompoundEdit;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import javax.swing.undo.AbstractUndoableEdit;
import javax.swing.undo.CompoundEdit;
import javax.swing.undo.UndoableEdit;


/**
 * Checks whether the CompoundEdit.lastEdit method
 * works correctly.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class lastEdit
  implements Testlet
{
  private static class MyCompoundEdit
    extends CompoundEdit
  {
    public UndoableEdit getLast()
    {
      return lastEdit();
    }
  }


  public void test(TestHarness harness)
  {
    MyCompoundEdit edit;
    AbstractUndoableEdit e1, e2;

    // Check #1.
    edit = new MyCompoundEdit();
    harness.check(edit.getLast() == null);

    // Check #2.
    edit.addEdit(e1 = new AbstractUndoableEdit());
    harness.check(edit.getLast() == e1);

    // Check #3.
    edit.addEdit(e2 = new AbstractUndoableEdit());
    harness.check(edit.getLast() == e2);
  }
}
