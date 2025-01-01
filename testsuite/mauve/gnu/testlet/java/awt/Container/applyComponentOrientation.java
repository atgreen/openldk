/* applyComponentOrientation.java -- some checks for the 
       applyComponentOrientation() method in the Container class.
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

package gnu.testlet.java.awt.Container;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.ComponentOrientation;
import java.awt.Container;
import java.awt.Label;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.List;

public class applyComponentOrientation 
  implements Testlet, PropertyChangeListener
{
  List events = new java.util.ArrayList();
  
  public void propertyChange(PropertyChangeEvent e) 
  {
    events.add(e);
  }

  public void test(TestHarness harness) 
  {
    Container c = new Container();
    Label l1 = new Label("ABC");
    Container c1 = new Container();
    Label l2 = new Label("DEF");
    Container c2 = new Container();
    Label l3 = new Label("GHI");
    c.add(l1);
    c.add(c1);
    c1.add(l2);
    c1.add(c2);
    c2.add(l3);
    harness.check(c.getComponentOrientation(), ComponentOrientation.UNKNOWN);
    harness.check(c1.getComponentOrientation(), ComponentOrientation.UNKNOWN);
    harness.check(c2.getComponentOrientation(), ComponentOrientation.UNKNOWN);
    harness.check(l1.getComponentOrientation(), ComponentOrientation.UNKNOWN);
    harness.check(l2.getComponentOrientation(), ComponentOrientation.UNKNOWN);
    harness.check(l3.getComponentOrientation(), ComponentOrientation.UNKNOWN);
    
    c.addPropertyChangeListener(this);
    c1.addPropertyChangeListener(this);
    c2.addPropertyChangeListener(this);
    l1.addPropertyChangeListener(this);
    l2.addPropertyChangeListener(this);
    l3.addPropertyChangeListener(this);
    c.applyComponentOrientation(ComponentOrientation.LEFT_TO_RIGHT);
    harness.check(c.getComponentOrientation(), 
            ComponentOrientation.LEFT_TO_RIGHT);
    harness.check(c1.getComponentOrientation(), 
            ComponentOrientation.LEFT_TO_RIGHT);
    harness.check(c2.getComponentOrientation(), 
            ComponentOrientation.LEFT_TO_RIGHT);
    harness.check(l1.getComponentOrientation(), 
            ComponentOrientation.LEFT_TO_RIGHT);
    harness.check(l2.getComponentOrientation(), 
            ComponentOrientation.LEFT_TO_RIGHT);
    harness.check(l3.getComponentOrientation(), 
            ComponentOrientation.LEFT_TO_RIGHT);
    
    harness.check(events.size(), 6);
    PropertyChangeEvent e0 = (PropertyChangeEvent) events.get(0);
    harness.check(e0.getSource(), c);
    harness.check(e0.getPropertyName(), "componentOrientation");
    harness.check(e0.getOldValue(), ComponentOrientation.UNKNOWN);
    harness.check(e0.getNewValue(), ComponentOrientation.LEFT_TO_RIGHT);

    PropertyChangeEvent e1 = (PropertyChangeEvent) events.get(1);
    harness.check(e1.getSource(), l1);
    harness.check(e1.getPropertyName(), "componentOrientation");
    harness.check(e1.getOldValue(), ComponentOrientation.UNKNOWN);
    harness.check(e1.getNewValue(), ComponentOrientation.LEFT_TO_RIGHT);

    PropertyChangeEvent e2 = (PropertyChangeEvent) events.get(2);
    harness.check(e2.getSource(), c1);
    harness.check(e2.getPropertyName(), "componentOrientation");
    harness.check(e2.getOldValue(), ComponentOrientation.UNKNOWN);
    harness.check(e2.getNewValue(), ComponentOrientation.LEFT_TO_RIGHT);

    PropertyChangeEvent e3 = (PropertyChangeEvent) events.get(3);
    harness.check(e3.getSource(), l2);
    harness.check(e3.getPropertyName(), "componentOrientation");
    harness.check(e3.getOldValue(), ComponentOrientation.UNKNOWN);
    harness.check(e3.getNewValue(), ComponentOrientation.LEFT_TO_RIGHT);

    PropertyChangeEvent e4 = (PropertyChangeEvent) events.get(4);
    harness.check(e4.getSource(), c2);
    harness.check(e4.getPropertyName(), "componentOrientation");
    harness.check(e4.getOldValue(), ComponentOrientation.UNKNOWN);
    harness.check(e4.getNewValue(), ComponentOrientation.LEFT_TO_RIGHT);  

    PropertyChangeEvent e5 = (PropertyChangeEvent) events.get(5);
    harness.check(e5.getSource(), l3);
    harness.check(e5.getPropertyName(), "componentOrientation");
    harness.check(e5.getOldValue(), ComponentOrientation.UNKNOWN);
    harness.check(e5.getNewValue(), ComponentOrientation.LEFT_TO_RIGHT);
    
    // try null
    boolean pass = false;
    try
    {
      c.applyComponentOrientation(null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
    
  }

}
