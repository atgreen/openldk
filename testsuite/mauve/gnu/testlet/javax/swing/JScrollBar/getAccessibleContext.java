/* getAccessibleContext.java -- some checks for the getAccessibleContext()
       method in the JScrollBar class.
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

package gnu.testlet.javax.swing.JScrollBar;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.List;

import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.accessibility.AccessibleState;
import javax.accessibility.AccessibleStateSet;
import javax.accessibility.AccessibleValue;
import javax.swing.JScrollBar;
import javax.swing.JSlider;

public class getAccessibleContext implements Testlet, PropertyChangeListener
{
  List events = new java.util.ArrayList();
  
  public void propertyChange(PropertyChangeEvent e)
  {
    events.add(e);
  }
  
  public void test(TestHarness harness)
  {
    JScrollBar scrollBar = new JScrollBar(JScrollBar.HORIZONTAL, 50, 5, 10, 90);
    AccessibleContext ac = scrollBar.getAccessibleContext();
    harness.check(ac.getAccessibleName(), null);
    harness.check(ac.getAccessibleRole(), AccessibleRole.SCROLL_BAR);
    harness.check(ac.getAccessibleAction(), null);
    harness.check(ac.getAccessibleComponent(), ac);
    harness.check(ac.getAccessibleDescription(), null);
    harness.check(ac.getAccessibleEditableText(), null);
    harness.check(ac.getAccessibleIcon(), null);
    harness.check(ac.getAccessibleTable(), null);
    harness.check(ac.getAccessibleText(), null);
    
    // the AccessibleContext is also the AccessibleValue...
    AccessibleValue av = ac.getAccessibleValue();
    harness.check(av, ac);
    harness.check(av.getCurrentAccessibleValue(), new Integer(50));
    harness.check(av.getMinimumAccessibleValue(), new Integer(10));
    harness.check(av.getMaximumAccessibleValue(), new Integer(85));
    
    // check that setting the accessible value updates the slider
    ac.addPropertyChangeListener(this);
    boolean b = av.setCurrentAccessibleValue(new Integer(55));
    harness.check(scrollBar.getValue(), 55);
    harness.check(b);
    harness.check(events.size(), 1);
    PropertyChangeEvent e0 = (PropertyChangeEvent) events.get(0);
    harness.check(e0.getPropertyName(), 
        AccessibleContext.ACCESSIBLE_VALUE_PROPERTY);
    harness.check(e0.getSource(), ac);
    harness.check(e0.getOldValue(), new Integer(50));
    harness.check(e0.getNewValue(), new Integer(55));
    
    // set the value below the minimum
    events.clear();
    b = av.setCurrentAccessibleValue(new Integer(5));
    harness.check(av.getCurrentAccessibleValue(), new Integer(10));
    harness.check(b);
    harness.check(events.size(), 1);
    e0 = (PropertyChangeEvent) events.get(0);
    harness.check(e0.getPropertyName(), 
        AccessibleContext.ACCESSIBLE_VALUE_PROPERTY);
    harness.check(e0.getSource(), ac);
    harness.check(e0.getOldValue(), new Integer(55));
    harness.check(e0.getNewValue(), new Integer(10));
    
    // set the value above the maximum
    events.clear();
    b = av.setCurrentAccessibleValue(new Integer(105));
    harness.check(av.getCurrentAccessibleValue(), new Integer(85));
    harness.check(b);
    harness.check(events.size(), 1);
    e0 = (PropertyChangeEvent) events.get(0);
    harness.check(e0.getPropertyName(), 
        AccessibleContext.ACCESSIBLE_VALUE_PROPERTY);
    harness.check(e0.getSource(), ac);
    harness.check(e0.getOldValue(), new Integer(10));
    harness.check(e0.getNewValue(), new Integer(85));
    
    // set the value to null
    events.clear();
    b = av.setCurrentAccessibleValue(null);
    harness.check(av.getCurrentAccessibleValue(), new Integer(85));
    harness.check(events.size(), 0);
    harness.check(!b);
    
    // check the state settings...
    AccessibleStateSet set = ac.getAccessibleStateSet();
    harness.check(set.contains(AccessibleState.ENABLED));
    harness.check(set.contains(AccessibleState.FOCUSABLE));
    harness.check(set.contains(AccessibleState.VISIBLE));
    harness.check(set.contains(AccessibleState.OPAQUE));
    harness.check(set.contains(AccessibleState.HORIZONTAL));
    
    // each call creates a new set...
    AccessibleStateSet set2 = ac.getAccessibleStateSet();
    harness.check(set != set2);
    
    // check the orientation state setting...
    scrollBar.setOrientation(JSlider.VERTICAL);
    set = ac.getAccessibleStateSet();
    harness.check(set.contains(AccessibleState.VERTICAL));
  }
}