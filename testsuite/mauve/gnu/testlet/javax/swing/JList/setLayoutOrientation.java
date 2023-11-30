/* setLayoutOrientation.java -- some checks for the setLayoutOrientation() 
       method in the JList class.
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

package gnu.testlet.javax.swing.JList;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.List;

import javax.swing.JList;

public class setLayoutOrientation implements Testlet, PropertyChangeListener
{
  List events = new java.util.ArrayList();

  public void propertyChange(PropertyChangeEvent e) 
  {
    events.add(e);
  }  
  
  public void test(TestHarness harness)
  {
    JList list = new JList();
    list.addPropertyChangeListener(this);
    harness.check(list.getLayoutOrientation(), JList.VERTICAL);
    list.setLayoutOrientation(JList.HORIZONTAL_WRAP);
    harness.check(list.getLayoutOrientation(), JList.HORIZONTAL_WRAP);
    harness.check(events.size(), 1);
    PropertyChangeEvent e0 = (PropertyChangeEvent) events.get(0);
    harness.check(e0.getSource(), list);
    harness.check(e0.getPropertyName(), "layoutOrientation");
    harness.check(e0.getOldValue(), new Integer(JList.VERTICAL));
    harness.check(e0.getNewValue(), new Integer(JList.HORIZONTAL_WRAP));
    
    // setting the same value again generates no event
    events.clear();
    list.setLayoutOrientation(JList.HORIZONTAL_WRAP);
    harness.check(events.size(), 0);
    
    // try a bad value
    boolean pass = false;
    try
    {
      list.setLayoutOrientation(99);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
  }

}
