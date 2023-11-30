/* setModel.java -- Checks for the setModel() method in the JSpinner class.
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
import java.util.List;

import javax.swing.JSpinner;
import javax.swing.SpinnerDateModel;
import javax.swing.SpinnerNumberModel;

public class setModel implements Testlet, PropertyChangeListener 
{
   List events = new java.util.ArrayList();
   
   public void propertyChange(PropertyChangeEvent e) 
   {
     events.add(e);
   }
   
   public void test(TestHarness harness) 
   {
     harness.checkPoint("(SpinnerModel)");
     JSpinner s = new JSpinner();
     harness.check(s.getEditor() instanceof JSpinner.NumberEditor);
     s.addPropertyChangeListener(this);
     SpinnerDateModel m = new SpinnerDateModel();
     s.setModel(m);
     harness.check(s.getModel(), m);
     harness.check(events.size(), 2);
     PropertyChangeEvent e1 = (PropertyChangeEvent) events.get(0);
     harness.check(e1.getPropertyName(), "model");
     harness.check(e1.getSource(), s);
     harness.check(e1.getNewValue(), m);
     harness.check(e1.getOldValue() instanceof SpinnerNumberModel);
     PropertyChangeEvent e2 = (PropertyChangeEvent) events.get(1);
     harness.check(e2.getPropertyName(), "editor");
     harness.check(e2.getSource(), s);
     harness.check(e2.getNewValue() instanceof JSpinner.DateEditor);
     harness.check(e2.getOldValue() instanceof JSpinner.NumberEditor);
   
     // setting the same model generates no event
     events.clear();
     s.setModel(m);
     harness.check(events.size(), 0);
     
     // try null argument
     boolean pass = false;
     try
       {
         s.setModel(null);
       }
     catch (IllegalArgumentException e) 
       {
         pass = true;
       }
     harness.check(pass);
   }
   
}
