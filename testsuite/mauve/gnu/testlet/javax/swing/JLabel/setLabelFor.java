/* setLabelFor.java -- some checks for the setLabelFor() method in the
       JLabel class.
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

// Tags: JDK1.2

package gnu.testlet.javax.swing.JLabel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JLabel;

public class setLabelFor implements Testlet, PropertyChangeListener
{
  List events = new ArrayList();

  public void propertyChange(PropertyChangeEvent e) 
  {
    events.add(e);  
  }
  
  public void test(TestHarness harness) 
  {
    JButton b = new JButton("ABC");
    JLabel l = new JLabel("XYZ");
    b.addPropertyChangeListener(this);
    l.addPropertyChangeListener(this);
    l.setLabelFor(b);
    harness.check(l.getLabelFor(), b);
    harness.check(events.size(), 2);
    PropertyChangeEvent pce1 = (PropertyChangeEvent) events.get(0);
    harness.check(pce1.getSource(), l);
    harness.check(pce1.getPropertyName(), "labelFor");
    harness.check(pce1.getOldValue(), null);
    harness.check(pce1.getNewValue(), b);
    
    PropertyChangeEvent pce2 = (PropertyChangeEvent) events.get(1);
    harness.check(pce2.getSource(), b);
    harness.check(pce2.getPropertyName(), "labeledBy");
    harness.check(pce2.getOldValue(), null);
    harness.check(pce2.getNewValue(), l);
    
    harness.check(b.getClientProperty("labeledBy"), l);
    
    // setting the same component should generate no events
    events.clear();
    l.setLabelFor(b);
    harness.check(events.size(), 0);
    
    // try null
    events.clear();
    l.setLabelFor(null);
    harness.check(events.size(), 2);
  }
  
}
