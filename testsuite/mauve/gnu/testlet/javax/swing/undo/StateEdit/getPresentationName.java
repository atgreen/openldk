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
 * Checks whether the StateEdit.getPresentationName method
 * works correctly.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class getPresentationName
  implements Testlet
{
  public void test(TestHarness harness)
  {
    StateEdit edit;
    StateEditable edited = new StateEditable()
    {
      public void storeState(Hashtable state) { }
      public void restoreState(Hashtable state) { }
    }; 

    // Check #1.
    edit = new StateEdit(edited, "Ostracize");
    harness.check(edit.getPresentationName(), "Ostracize");

    // Check #2.
    edit = new StateEdit(edited);
    harness.check(edit.getPresentationName() == null);
  }
}
