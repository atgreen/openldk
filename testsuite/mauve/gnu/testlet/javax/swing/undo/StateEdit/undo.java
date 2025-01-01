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

package gnu.testlet.javax.swing.undo.StateEdit;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.util.Hashtable;
import javax.swing.undo.StateEdit;
import javax.swing.undo.StateEditable;


/**
 * Checks whether the StateEdit.undo method works correctly.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class undo
  implements Testlet
{
  private static class Person
    implements StateEditable
  {
    private String name;
    
    public String getName()
    {
      return name;
    }

    public void setName(String name)
    {
      this.name = name;
    }

    public void restoreState(Hashtable h)
    {
      if (h.containsKey("name"))
        setName((String) h.get("name"));
    }

    public void storeState(Hashtable h)
    {
      h.put("name", name);
    }
  }

  public void test(TestHarness harness)
  {
    StateEdit edit;
    Person p;

    // Check #1.
    p = new Person();
    p.setName("Daniel Dandelion");
    edit = new StateEdit(p, "Name Change");
    harness.check(p.getName(), "Daniel Dandelion");
    
    // Check #2.
    p.setName("Rose Rosenholz");
    edit.end();
    harness.check(p.getName(), "Rose Rosenholz");

    // Check #3.
    edit.undo();
    harness.check(p.getName(), "Daniel Dandelion");

    // Check #4.
    edit.redo();
    harness.check(p.getName(), "Rose Rosenholz");
  }
}
