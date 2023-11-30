/* setFont.java -- some checks for the setFont() method in the JLabel class.
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

package gnu.testlet.javax.swing.JLabel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Font;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.List;

import javax.swing.JLabel;

public class setFont implements Testlet, PropertyChangeListener
{
  List events = new java.util.ArrayList();
  
  public void propertyChange(PropertyChangeEvent e) 
  {
    events.add(e);
  }

  public void test(TestHarness harness) 
  {
    JLabel label = new JLabel("ABC");
    label.addPropertyChangeListener(this);
    Font f = new Font("Dialog", Font.PLAIN, 15);
    label.setFont(f);
    harness.check(label.getFont(), f);
    harness.check(events.size(), 1);
    PropertyChangeEvent e0 = (PropertyChangeEvent) events.get(0);
    harness.check(e0.getSource(), label);
    harness.check(e0.getPropertyName(), "font");
    harness.check(e0.getNewValue(), f);
    
    // try null
    events.clear();
    label.setFont(null);
    harness.check(label.getFont(), null);
    harness.check(events.size(), 1);
    e0 = (PropertyChangeEvent) events.get(0);
    harness.check(e0.getSource(), label);
    harness.check(e0.getPropertyName(), "font");
    harness.check(e0.getOldValue(), f);
    harness.check(e0.getNewValue(), null);
  }
  
}
