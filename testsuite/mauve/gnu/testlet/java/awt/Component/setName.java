/* setName.java -- some checks for the setName() method in the Component class.
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
import java.awt.Label;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.List;

public class setName implements Testlet, PropertyChangeListener
{
  List events = new java.util.ArrayList();
  
  public void propertyChange(PropertyChangeEvent e) 
  {
    events.add(e);
  }

  public void test(TestHarness harness)
  {
    Component c = new Label("ABC");
    harness.check(c.getName() != null);
    c.addPropertyChangeListener(this);
    c.setName("XYZ");
    harness.check(c.getName(), "XYZ");
    harness.check(events.size(), 1);
    PropertyChangeEvent e0 = (PropertyChangeEvent) events.get(0);
    harness.check(e0.getSource(), c);
    harness.check(e0.getPropertyName(), "name");
    harness.check(e0.getOldValue() != null);
    harness.check(e0.getNewValue(), "XYZ");
    
    // setting the same value generates no event
    events.clear();
    c.setName("XYZ");
    harness.check(events.size(), 0);
    
    // try null
    c.setName(null);
    harness.check(c.getName(), null);
  }
}
