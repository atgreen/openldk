// Tags: JDK1.4

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

package gnu.testlet.javax.swing.undo.UndoableEditSupport;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.undo.UndoableEditSupport;

/**
 * Checks whether the UndoableEditSupport.getUndoableEditListeners
 * method works correctly. That method has been added in JDK1.4.
 *
 * @author <a href="mailto:brawer@dandelis.ch">Sascha Brawer</a>
 */
public class getUndoableEditListeners
  implements Testlet
{
  public void test(TestHarness harness)
  {
    UndoableEditSupport ues;
    TestListener t1, t2;
    UndoableEditListener[] l;

    ues = new UndoableEditSupport();
    t1 = new TestListener();
    t2 = new TestListener();

    // Check #1.
    l = ues.getUndoableEditListeners();
    harness.check((l != null) && (l.length == 0));

    // Check #2.
    ues.addUndoableEditListener(t1);
    l = ues.getUndoableEditListeners();
    harness.check((l != null) && (l.length == 1) && (l[0] == t1));

    // Check #3.
    ues.addUndoableEditListener(t2);
    l = ues.getUndoableEditListeners();
    harness.check(l != null && l.length == 2
                  && l[0] == t1 && l[1] == t2);

    // Check #4.
    ues.removeUndoableEditListener(t1);
    l = ues.getUndoableEditListeners();
    harness.check(l != null && l.length == 1
                  && l[0] == t2);
  }


  private static class TestListener
    implements UndoableEditListener
  {
    public void undoableEditHappened(UndoableEditEvent evt)
    {
    }
  }
}
