/* setMinimumSize.java -- some checks for the setMinimumSize() method in the
       Component class.
   Copyright (C) 2006 David Gilbert  <david.gilbert@object-refinery.com>
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

// Tags: JDK1.5

package gnu.testlet.java.awt.Component;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Button;
import java.awt.Component;
import java.awt.Dimension;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.List;

public class setMinimumSize implements Testlet, PropertyChangeListener 
{

  List events = new java.util.ArrayList();
  
  public void propertyChange(PropertyChangeEvent event) 
  {
    events.add(event);
  }

  public void test(TestHarness harness) 
  {
    Component c = new Button("ABC");
    harness.check(c.getMinimumSize(), new Dimension(0, 0));
    harness.check(c.isMinimumSizeSet(), false);
    c.addPropertyChangeListener(this);
    c.setMinimumSize(new Dimension(10, 20));
    harness.check(c.getMinimumSize(), new Dimension(10, 20));
    harness.check(events.size(), 1);
    PropertyChangeEvent e = (PropertyChangeEvent) events.get(0);
    harness.check(e.getPropertyName(), "minimumSize");
    harness.check(e.getSource(), c);
    harness.check(e.getOldValue(), null);
    harness.check(e.getNewValue(), new Dimension(10, 20));
    
    events.clear();
    c.setMinimumSize(new Dimension(30, 40));
    harness.check(c.getMinimumSize(), new Dimension(30, 40));
    harness.check(c.isMinimumSizeSet(), true);
    harness.check(events.size(), 1);
    e = (PropertyChangeEvent) events.get(0);
    harness.check(e.getPropertyName(), "minimumSize");
    harness.check(e.getSource(), c);
    harness.check(e.getOldValue(), new Dimension(10, 20));
    harness.check(e.getNewValue(), new Dimension(30, 40));
    
    events.clear();
    c.setMinimumSize(null);
    harness.check(c.getMinimumSize(), new Dimension(0, 0));
    harness.check(c.isMinimumSizeSet(), false);
    harness.check(events.size(), 1);
    e = (PropertyChangeEvent) events.get(0);
    harness.check(e.getPropertyName(), "minimumSize");
    harness.check(e.getSource(), c);
    harness.check(e.getOldValue(), new Dimension(30, 40));
    harness.check(e.getNewValue(), null);
    
  }

}
