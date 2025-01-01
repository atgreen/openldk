/* stateChanged.java -- Checks for the stateChanged() method in the 
                        DefaultEditor class.
   Copyright (C) 2006 David Gilbert <david.gilbert@object-refinery.com>
This file is part of Mauve.

Mauve is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Mauve is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Mauve; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

*/

// Tags: JDK1.4
// Uses: MyDefaultEditor

package gnu.testlet.javax.swing.JSpinner.DefaultEditor;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.JSpinner;
import javax.swing.event.ChangeEvent;

public class stateChanged implements Testlet 
{

  public void test(TestHarness harness) 
  {
    harness.checkPoint("(ChangeEvent)");
    JSpinner s = new JSpinner();
    MyDefaultEditor e = new MyDefaultEditor(s);
    s.setEditor(e);
    s.getModel().setValue(new Integer(99));
    harness.check(e.stateChangeEvents.size(), 1);
    ChangeEvent event = (ChangeEvent) e.stateChangeEvents.get(0);
    harness.check(event.getSource(), s);
    harness.check(e.getTextField().getText(), "99");
  }
}
