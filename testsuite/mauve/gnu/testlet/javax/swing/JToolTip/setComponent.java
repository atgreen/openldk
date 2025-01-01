/* setComponent.java -- some checks for the setComponent()
       method in the JToolTip class.
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

package gnu.testlet.javax.swing.JToolTip;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JToolTip;

public class setComponent 
  implements Testlet, PropertyChangeListener
{
  List events = new java.util.ArrayList();
  
  public void propertyChange(PropertyChangeEvent event)
  {
    events.add(event);
  }
  
  public void test(TestHarness harness)
  {
    JButton component = new JButton("Button");
    JToolTip tt = new JToolTip();
    tt.addPropertyChangeListener(this);
    tt.setComponent(component);
    harness.check(tt.getComponent(), component);

    // check the generated event...
    harness.check(events.size(), 1);
    PropertyChangeEvent e = (PropertyChangeEvent) events.get(0);
    harness.check(e.getPropertyName(), "component");
    harness.check(e.getSource(), tt);
    harness.check(e.getOldValue(), null);
    harness.check(e.getNewValue(), component);
    
    // check that the component can be set to null
    events.clear();
    tt.setComponent(null);
    harness.check(events.size(), 1);
    e = (PropertyChangeEvent) events.get(0);
    harness.check(e.getPropertyName(), "component");
    harness.check(e.getSource(), tt);
    harness.check(e.getOldValue(), component);
    harness.check(e.getNewValue(), null);
  }
}
