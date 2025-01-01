/* setComponentOrientation.java -- some checks for the 
       setComponentOrientation() method in the Component class.
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

package gnu.testlet.java.awt.Component;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Component;
import java.awt.ComponentOrientation;
import java.awt.Label;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.List;

public class setComponentOrientation implements Testlet, PropertyChangeListener
{
  List events = new java.util.ArrayList();
  
  public void propertyChange(PropertyChangeEvent e) 
  {
    events.add(e);
  }

  public void test(TestHarness harness)
  {
    Component c = new Label("ABC");
    harness.check(c.getComponentOrientation(), ComponentOrientation.UNKNOWN);
    c.addPropertyChangeListener(this);
    c.setComponentOrientation(ComponentOrientation.LEFT_TO_RIGHT);
    harness.check(c.getComponentOrientation(), ComponentOrientation.LEFT_TO_RIGHT);
    harness.check(events.size(), 1);
    PropertyChangeEvent e = (PropertyChangeEvent) events.get(0);
    harness.check(e.getSource(), c);
    harness.check(e.getPropertyName(), "componentOrientation");
    harness.check(e.getOldValue(), ComponentOrientation.UNKNOWN);
    harness.check(e.getNewValue(), ComponentOrientation.LEFT_TO_RIGHT);
    
    // setting the same value again generates no event
    events.clear();
    c.setComponentOrientation(ComponentOrientation.LEFT_TO_RIGHT);
    harness.check(events.size(), 0);
    
    // try null
    c.setComponentOrientation(null);
    harness.check(c.getComponentOrientation(), null);
    harness.check(events.size(), 1);
    e = (PropertyChangeEvent) events.get(0);
    harness.check(e.getSource(), c);
    harness.check(e.getPropertyName(), "componentOrientation");
    harness.check(e.getOldValue(), ComponentOrientation.LEFT_TO_RIGHT);
    harness.check(e.getNewValue(), null);
  }
  
}
