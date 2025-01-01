/* setComponentPopupMenu.java -- some checks for the setComponentPopupMenu()
       method in the JComponent class.
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

// Tags: JDK1.5

package gnu.testlet.javax.swing.JComponent;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JPopupMenu;

public class setComponentPopupMenu implements Testlet, PropertyChangeListener
{
  List events = new java.util.ArrayList();
    
  public void propertyChange(PropertyChangeEvent e) 
  {
    events.add(e);
  }

  public void test(TestHarness harness) 
  {
    JComponent c = new JButton("ABC");
    JPopupMenu p = new JPopupMenu();
    harness.check(c.getComponentPopupMenu(), null);
    c.addPropertyChangeListener(this);
    c.setComponentPopupMenu(p);
    harness.check(c.getComponentPopupMenu(), p);
    harness.check(events.size(), 1);
    PropertyChangeEvent e = (PropertyChangeEvent) events.get(0);
    harness.check(e.getSource(), c);
    harness.check(e.getPropertyName(), "componentPopupMenu");
    harness.check(e.getOldValue(), null);
    harness.check(e.getNewValue(), p);
     
    // same value should generate no event
    events.clear();
    c.setComponentPopupMenu(p);
    harness.check(events.size(), 0);
  }

}
