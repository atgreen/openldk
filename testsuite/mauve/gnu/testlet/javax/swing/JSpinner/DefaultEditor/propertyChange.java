/* propertyChange.java -- Checks for the propertyChange() method in the 
                          JSpinner.DefaultEditor class.
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

// Tags: FIXME
// Uses: MyDefaultEditor

package gnu.testlet.javax.swing.JSpinner.DefaultEditor;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.beans.PropertyChangeEvent;

import javax.swing.JSpinner;

public class propertyChange implements Testlet 
{

  public void test(TestHarness harness) 
  {
    harness.checkPoint("(PropertyChangeEvent)");
    JSpinner s = new JSpinner();
    MyDefaultEditor e = new MyDefaultEditor(s);
    s.setEditor(e);
    e.propertyChangeEvents.clear();
    e.getTextField().setValue(new Integer(88));
    harness.check(e.propertyChangeEvents.size(), 1);
    PropertyChangeEvent event = (PropertyChangeEvent) 
        e.propertyChangeEvents.get(0);
    harness.check(event.getPropertyName(), "value");
    harness.check(e.getTextField().getText(), "88");
  }
}
