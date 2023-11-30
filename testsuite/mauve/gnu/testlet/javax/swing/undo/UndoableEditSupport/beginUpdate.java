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

import javax.swing.undo.CompoundEdit;
import javax.swing.undo.UndoableEditSupport;


/**
 * @see Classpath bug #7109.
 * @author <a href="mailto:brawer@dandelis.ch">Sascha Brawer</a>
 */
public class beginUpdate
  implements Testlet
{
  public void test(TestHarness harness)
  {
    MyUES ues;
    CompoundEdit cedit;

    ues = new MyUES();

    // Check #1 and #2: Pre-requisites.
    harness.check(ues.getCompoundEdit(), null);
    harness.check(ues.getUpdateLevel(), 0);

    // Check #3.
    ues.beginUpdate();
    cedit = ues.getCompoundEdit();
    harness.check(ues.getCompoundEdit() instanceof MyCompoundEdit);

    // Check #4.
    harness.check(ues.getUpdateLevel(), 1);

    // Check #5.
    ues.beginUpdate();
    harness.check(ues.getCompoundEdit() == cedit);
    
    // Check #6.
    harness.check(ues.getUpdateLevel(), 2);

    // Check #7.
    ues.beginUpdate();
    harness.check(ues.getCompoundEdit() == cedit);
    
    // Check #8.
    harness.check(ues.getUpdateLevel(), 3);

    // Check #9.
    ues.endUpdate();
    harness.check(ues.getUpdateLevel(), 2);

    // Check #10.
    ues.endUpdate();
    harness.check(ues.getUpdateLevel(), 1);

    // Check #11.
    ues.beginUpdate();
    harness.check(ues.getCompoundEdit() == cedit);

    // Check #12.
    harness.check(ues.getUpdateLevel(), 2);

    // Check #13.
    ues.endUpdate();
    harness.check(ues.getUpdateLevel(), 1);

    // Check #14.
    ues.endUpdate();
    harness.check(ues.getCompoundEdit(), null);

    // Check #15.
    harness.check(ues.getUpdateLevel(), 0);

    // Check #16.
    ues.beginUpdate();
    harness.check(ues.getUpdateLevel(), 1);

    // Check #17: Use a fresh CompoundEdit when transitioning
    // undo level from 0 to 1.
    harness.check(ues.getCompoundEdit() != cedit);
  }


  private static class MyUES
    extends UndoableEditSupport
  {
    protected CompoundEdit createCompoundEdit()
    {
      return new MyCompoundEdit();
    }

    public CompoundEdit getCompoundEdit()
    {
      return compoundEdit;
    }
  }


  private static class MyCompoundEdit
    extends CompoundEdit
  {
  }
}
