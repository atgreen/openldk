/* setVerifyInputWhenFocusTarget.java -- some checks for the 
       setVerifyInputWhenFocusTarget() method in the JComponent class.
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

// Tags: JDK1.3

package gnu.testlet.javax.swing.JComponent;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JComponent;

public class setVerifyInputWhenFocusTarget 
  implements Testlet, PropertyChangeListener 
{
  List events = new java.util.ArrayList();
  
  public void propertyChange(PropertyChangeEvent e) 
  {
    events.add(e);
  }

  public void test(TestHarness harness) 
  {
    JComponent c = new JButton("ABC");
    harness.check(c.getVerifyInputWhenFocusTarget(), true);
    c.addPropertyChangeListener(this);
    c.setVerifyInputWhenFocusTarget(false);
    harness.check(c.getVerifyInputWhenFocusTarget(), false);
    harness.check(events.size(), 1);
    PropertyChangeEvent e0 = (PropertyChangeEvent) events.get(0);
    harness.check(e0.getSource(), c);
    harness.check(e0.getPropertyName(), "verifyInputWhenFocusTarget");
    harness.check(e0.getOldValue(), Boolean.TRUE);
    harness.check(e0.getNewValue(), Boolean.FALSE);
    
    // setting the same value generates no event
    events.clear();
    c.setVerifyInputWhenFocusTarget(false);
    harness.check(events.size(), 0);
  }

}
