/* constructor.java -- Checks for the constructor in the ListEditor class.
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

package gnu.testlet.javax.swing.JSpinner.ListEditor;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.beans.PropertyChangeListener;
import java.util.Arrays;
import java.util.EventListener;

import javax.swing.JSpinner;
import javax.swing.SpinnerListModel;
import javax.swing.event.ChangeListener;

public class constructor implements Testlet
{
  public void test(TestHarness harness) 
  {
    harness.checkPoint("(JSpinner)");
    SpinnerListModel m = new SpinnerListModel(new String[] {"A", "B", "C"});
    JSpinner s = new JSpinner(m);
    JSpinner.DefaultEditor e = new JSpinner.DefaultEditor(s);
    harness.check(e.getLayout(), e);
    harness.check(e.getTextField().getValue(), "A");
    
    // the editor should be a listener on the spinner
    EventListener[] sl = s.getListeners(ChangeListener.class);
    harness.check(Arrays.asList(sl).contains(e));
    
    // the editor should be listening to PropertyChangeEvents in the
    // text field
    EventListener[] tfl = e.getTextField().getListeners(
            PropertyChangeListener.class);
    harness.check(Arrays.asList(tfl).contains(e));
  }
}
