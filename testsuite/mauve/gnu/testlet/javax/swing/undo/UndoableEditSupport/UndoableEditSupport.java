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

package gnu.testlet.javax.swing.undo.UndoableEditSupport;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import javax.swing.undo.CompoundEdit;


/**
 * @author <a href="mailto:brawer@dandelis.ch">Sascha Brawer</a>
 */
public class UndoableEditSupport
  implements Testlet
{
  public void test(TestHarness harness)
  {
    MySupport ms;
    Object foo = "foo";

    ms = new MySupport();
    harness.check(ms.getRealSource() == ms);   // #1
    harness.check(ms.getUpdateLevel(), 0);     // #2
    harness.check(ms.getCompoundEdit(), null); // #3

    ms = new MySupport(null);
    harness.check(ms.getRealSource() == ms);   // #4
    harness.check(ms.getUpdateLevel(), 0);     // #5
    harness.check(ms.getCompoundEdit(), null); // #6

    ms = new MySupport(foo);
    harness.check(ms.getRealSource() == foo);  // #7
    harness.check(ms.getUpdateLevel(), 0);     // #8
    harness.check(ms.getCompoundEdit(), null); // #9
  }


  private static class MySupport
    extends javax.swing.undo.UndoableEditSupport
  {
    public MySupport()
    {
      super();
    }

    public MySupport(Object realSource)
    {
      super(realSource);
    }

    public Object getRealSource()
    {
      return realSource;
    }

    public CompoundEdit getCompoundEdit()
    {
      return compoundEdit;
    }
  }
}
  
