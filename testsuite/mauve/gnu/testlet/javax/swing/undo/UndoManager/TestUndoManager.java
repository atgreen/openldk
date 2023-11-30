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

import java.util.Vector;
import javax.swing.undo.AbstractUndoableEdit;
import javax.swing.undo.UndoableEdit;
import javax.swing.undo.UndoManager;


/**
 * An UndoManager that exposes various protected data structures,
 * which makes it easier to ensure that the internal state is
 * correct after calling public methods.
 *
 * @author <a href="mailto:brawer@dandelis.ch">Sascha Brawer</a>
 */
public class TestUndoManager
  extends UndoManager
{
  public int numTrimForLimitCalls;

  protected void trimForLimit()
  {
    ++numTrimForLimitCalls;
    super.trimForLimit();
  }

  public Vector getEdits()
  {
    return edits;
  }

  public void invokeTrimEdits(int from, int to)
  {
    // UndoManager.trimEdits is protected
    trimEdits(from, to);
  }

  public void invokeRedoTo(UndoableEdit edit)
  {
    // UndoManager.redoTo is protected
    redoTo(edit);
  }

  public void invokeUndoTo(UndoableEdit edit)
  {
    // UndoManager.undoTo is protected
    undoTo(edit);
  }

  public UndoableEdit getEditToBeRedone()
  {
    // UndoManager.editToBeRedone is protected
    return editToBeRedone();
  }

  public UndoableEdit getEditToBeUndone()
  {
    // UndoManager.editToBeUndone is protected
    return editToBeUndone();
  }

  public TestEdit[] addTestEdits(int firstID, int count)
  {
    TestEdit[] result = new TestEdit[count];
    for (int i = 0; i < count; i++)
    {
      result[i] = new TestEdit(firstID + i);
      addEdit(result[i]);
    }
    return result;
  }


  public boolean checkIDs(int[] expected)
  {
    if (expected.length != edits.size())
      return false;

    for (int i = 0; i < expected.length; i++)
      if (expected[i] != ((TestEdit) edits.get(i)).id)
        return false;

    return true;
  }


  public static class TestEdit
    extends AbstractUndoableEdit
  {
    public final int id;
    private boolean significant = true;
    private boolean inhibitCanRedo = false;
    private boolean inhibitCanUndo = false;

    public TestEdit(int id)
    {
      this.id = id;
    }

    public boolean isAlive()
    {
      return canRedo() || canUndo();
    }

    public boolean isSignificant()
    {
      return significant;
    }


    public void setSignificant(boolean significant)
    {
      this.significant = significant;
    }

    public void inhibitCanUndo()
    {
      inhibitCanUndo = true;
    }

    public void inhibitCanRedo()
    {
      inhibitCanRedo = true;
    }

    public String toString()
    {
      return super.toString() + " id: " + id;
    }

    public boolean canRedo()
    {
      if (inhibitCanRedo)
        return false;
      else
        return super.canRedo();
    }

    public boolean canUndo()
    {
      if (inhibitCanUndo)
        return false;
      else
        return super.canUndo();
    }

    public String getUndoPresentationName()
    {
      return "UndoPres" + id;
    }

    public String getRedoPresentationName()
    {
      return "RedoPres" + id;
    }
  }
}
