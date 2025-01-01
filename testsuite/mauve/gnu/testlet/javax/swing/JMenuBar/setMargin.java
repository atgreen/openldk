/* setMargin.java -- some checks for the setMargin() method in the JMenuBar
       class.
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

package gnu.testlet.javax.swing.JMenuBar;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Insets;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.List;

import javax.swing.JMenuBar;

public class setMargin implements Testlet, PropertyChangeListener 
{
  List events = new java.util.ArrayList();
  
  public void propertyChange(PropertyChangeEvent e) 
  {
    events.add(e);
  }

  public void test(TestHarness harness) 
  {
    JMenuBar mb = new JMenuBar();
    harness.check(mb.getMargin(), new Insets(0, 0, 0, 0));
    harness.check(mb.getMargin() != mb.getMargin());
    mb.addPropertyChangeListener(this);
    mb.setMargin(new Insets(1, 2, 3, 4));
    harness.check(mb.getMargin(), new Insets(1, 2, 3, 4));
    harness.check(events.size(), 1);
    PropertyChangeEvent e = (PropertyChangeEvent) events.get(0);
    harness.check(e.getSource(), mb);
    harness.check(e.getPropertyName(), "margin");
    harness.check(e.getNewValue(), new Insets(1, 2, 3, 4));
    harness.check(e.getOldValue(), null);
    
    // setting the same value generates no event
    events.clear();
    mb.setMargin(new Insets(1, 2, 3, 4));
    harness.check(events.size(), 0);
    
    // set to null
    mb.setMargin(null);
    harness.check(mb.getMargin(), new Insets(0, 0, 0, 0));
    harness.check(events.size(), 1);
    e = (PropertyChangeEvent) events.get(0);
    harness.check(e.getSource(), mb);
    harness.check(e.getPropertyName(), "margin");
    harness.check(e.getNewValue(), null);
    harness.check(e.getOldValue(), new Insets(1, 2, 3, 4));
    
  }

  
}
