/* setResizeWeight.java -- Checks for the setResizeWeight() method in the 
       JSplitPane class.
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

package gnu.testlet.javax.swing.JSplitPane;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.List;

import javax.swing.JSplitPane;

public class setResizeWeight implements Testlet, PropertyChangeListener
{
  List events = new java.util.ArrayList();
  
  public void propertyChange(PropertyChangeEvent event)
  {
    events.add(event);
  }
  
  public void test(TestHarness harness)
  {
    JSplitPane s = new JSplitPane();
    s.addPropertyChangeListener(this);
    s.setResizeWeight(0.33);
    harness.check(s.getResizeWeight(), 0.33);
    harness.check(events.size(), 1);
    PropertyChangeEvent pce = (PropertyChangeEvent) events.get(0);
    harness.check(pce.getPropertyName(), "resizeWeight");
    harness.check(pce.getSource(), s);
    harness.check(pce.getOldValue(), new Double(0.0));
    harness.check(pce.getNewValue(), new Double(0.33));
    
    // check that setting the same value does not generate an event
    events.clear();
    s.setResizeWeight(0.33);
    harness.check(events.size(), 0);
    
    // try a bad value
    boolean pass = false;
    try
      {
        s.setResizeWeight(-0.33);
      }
    catch (IllegalArgumentException e)
      {
        pass = true;  
      }
    harness.check(pass);
  }
}
