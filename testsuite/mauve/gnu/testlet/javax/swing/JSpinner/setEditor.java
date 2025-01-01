/* setEditor.java -- Checks for the setEditor() method in the JSpinner class.
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

package gnu.testlet.javax.swing.JSpinner;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Arrays;
import java.util.EventListener;
import java.util.List;

import javax.swing.JComponent;
import javax.swing.JSpinner;
import javax.swing.SpinnerNumberModel;
import javax.swing.event.ChangeListener;

public class setEditor implements Testlet, PropertyChangeListener 
{

   List events = new java.util.ArrayList();
   
   public void propertyChange(PropertyChangeEvent e) 
   {
     events.add(e);
   }
    
  public void test(TestHarness harness)
  {
    harness.checkPoint("(JComponent)");
    JSpinner s = new JSpinner();
    s.addPropertyChangeListener(this);
    JComponent e = s.getEditor();
    SpinnerNumberModel m = (SpinnerNumberModel) s.getModel();
    
    EventListener[] l1 = s.getListeners(ChangeListener.class);
    harness.check(Arrays.asList(l1).contains(e));
    harness.check(!Arrays.asList(l1).contains(m));

    JSpinner.NumberEditor e2 = new JSpinner.NumberEditor(s);
    s.setEditor(e2);
    harness.check(s.getEditor(), e2);
    harness.check(events.size(), 1);
    PropertyChangeEvent pce = (PropertyChangeEvent) events.get(0);
    harness.check(pce.getPropertyName(), "editor");
    harness.check(pce.getOldValue(), e);
    harness.check(pce.getNewValue(), e2);
    l1 = s.getListeners(ChangeListener.class);
    harness.check(Arrays.asList(l1).contains(e2));
    harness.check(!Arrays.asList(l1).contains(e));
    
    // try null argument
    boolean pass = false;
    try
      {
        s.setEditor(null);
      }
    catch (IllegalArgumentException iae)
      {
        pass = true;
      }
    harness.check(pass);
  }

}
