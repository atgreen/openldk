// Tags: JDK1.2

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

package gnu.testlet.javax.swing.undo.UndoableEditSupport;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import javax.swing.event.UndoableEditEvent;
import javax.swing.undo.CompoundEdit;
import javax.swing.undo.UndoableEditSupport;


/**
 * @author <a href="mailto:brawer@dandelis.ch">Sascha Brawer</a>
 */
public class toString
  implements Testlet
{
  public void test(TestHarness harness)
  {
    UndoableEditSupport u;

    // Check #1.
    u = new MyUndoableEditSupport("realSource");
    harness.check(u.toString(),
                  u.getClass().getName() + "@"
                  + Integer.toHexString(u.hashCode())
                  + " updateLevel: 0 listeners: []"
                  + " compoundEdit: null");

    // Check #2.
    u.beginUpdate();
    u.beginUpdate();
    u.addUndoableEditListener(new MyListener());
    u.addUndoableEditListener(new MyListener());
    harness.check(u.toString(),
                  u.getClass().getName() + "@"
                  + Integer.toHexString(u.hashCode())
                  + " updateLevel: 2 listeners: [lily, lily]"
                  + " compoundEdit: rose");

    // Check #3: u.realSource == u.
    // Classpath bug #7119.
    u = new MyUndoableEditSupport(null);
    harness.check(u.toString(),
                  u.getClass().getName() + "@"
                  + Integer.toHexString(u.hashCode())
                  + " updateLevel: 0 listeners: []"
                  + " compoundEdit: null");
  }


  private static class MyUndoableEditSupport
    extends UndoableEditSupport
  {
    public MyUndoableEditSupport(Object realSource)
    {
      super(realSource);
    }

    public CompoundEdit createCompoundEdit()
    {
      return new CompoundEdit()
      {
        public String toString()
        {
          return "rose";
        }
      };
    }
  }

  private static class MyListener
    implements javax.swing.event.UndoableEditListener
  {
    public String toString()
    {
      return "lily";
    }

    public void undoableEditHappened(UndoableEditEvent x)
    {
    }
  }
}

