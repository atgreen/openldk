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

import java.util.Vector;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import javax.swing.undo.*;


/**
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class addEdit
  implements Testlet
{
  private class E1
    extends AbstractUndoableEdit
  {
  }

  private class E2
    extends AbstractUndoableEdit
  {
    public boolean addEdit(UndoableEdit edit)
    {
      return edit instanceof E1;
    }
  }

  private class E3
    extends AbstractUndoableEdit
  {
    public boolean replaceEdit(UndoableEdit edit)
    {
      return edit instanceof E2;
    }
  }

  /**
   * A special subclass of CompoundEdit that allows retrieving the
   * protected list of UndoableEdits. This makes testing easier.
   */
  private class MyCompoundEdit
    extends CompoundEdit
  {
    public Vector getEdits()
    {
      return edits;
    }

    public UndoableEdit getLast()
    {
      return (UndoableEdit) lastEdit();
    }
  }

  public void test(TestHarness harness)
  {
    MyCompoundEdit edit = new MyCompoundEdit();
    Throwable caught;

    // Check #1.
    harness.check(edit.addEdit(new E1())
                  && (edit.getEdits().size() == 1));

    // Check #2.
    harness.check(edit.addEdit(new E2())
                  && (edit.getEdits().size() == 2));

    // Check #3. E2 should absorb E1.
    harness.check(edit.addEdit(new E1())
                  && (edit.getEdits().size() == 2)
                  && (edit.getLast() instanceof E2));

    // Check #4. E3 should replace E2.
    harness.check(edit.addEdit(new E3())
                  && (edit.getEdits().size() == 2)
                  && (edit.getLast() instanceof E3));

    // Check #5. E1 should not replace E3.
    harness.check(edit.addEdit(new E1())
                  && (edit.getEdits().size() == 3)
                  && (edit.getLast() instanceof E1));

    // Check #6. E3 should not replace E1.
    harness.check(edit.addEdit(new E3())
                  && (edit.getEdits().size() == 4)
                  && (edit.getLast() instanceof E3));

    // Check #7. Null does not taste good.
    caught = null;
    try
      {
        edit.addEdit(null);
      }
    catch (Exception ex)
      {
        caught = ex;
      }
    harness.check(caught != null);

    // Check #8. No further additions after calling end().
    edit.end();
    harness.check((edit.addEdit(new E1()) == false)
                  && (edit.getEdits().size() == 4));
  }
}
