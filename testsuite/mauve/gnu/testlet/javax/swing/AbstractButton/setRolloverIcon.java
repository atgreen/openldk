/* setRolloverIcon.java -- some checks for the setRolloverIcon() method in
       the AbstractButton class.
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

package gnu.testlet.javax.swing.AbstractButton;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.List;

import javax.swing.AbstractButton;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.plaf.metal.MetalIconFactory;

public class setRolloverIcon implements Testlet, PropertyChangeListener
{
  List events = new java.util.ArrayList();
  
  public void propertyChange(PropertyChangeEvent e) 
  {
    events.add(e);
  }

public void test(TestHarness harness)
  {
    Icon icon1 = MetalIconFactory.getFileChooserNewFolderIcon();
    Icon icon2 = MetalIconFactory.getHorizontalSliderThumbIcon();
    AbstractButton b = new JButton("123");
    b.setRolloverEnabled(false);
    b.addPropertyChangeListener(this);
    b.setRolloverIcon(icon1);
    harness.check(b.getRolloverIcon(), icon1);
    harness.check(events.size(), 2);
    PropertyChangeEvent e1 = (PropertyChangeEvent) events.get(0);
    harness.check(e1.getSource(), b);
    harness.check(e1.getPropertyName(), "rolloverIcon");
    harness.check(e1.getOldValue(), null);
    harness.check(e1.getNewValue(), icon1);
    PropertyChangeEvent e2 = (PropertyChangeEvent) events.get(1);
    harness.check(e2.getSource(), b);
    harness.check(e2.getPropertyName(), "rolloverEnabled");
    harness.check(e2.getOldValue(), Boolean.FALSE);
    harness.check(e2.getNewValue(), Boolean.TRUE);
    
    // change the icon
    events.clear();
    b.setRolloverIcon(icon2);
    harness.check(b.getRolloverIcon(), icon2);
    harness.check(events.size(), 1);
    e1 = (PropertyChangeEvent) events.get(0);
    harness.check(e1.getSource(), b);
    harness.check(e1.getPropertyName(), "rolloverIcon");
    harness.check(e1.getOldValue(), icon1);
    harness.check(e1.getNewValue(), icon2);
    
    // setting the same icon should generate no event
    events.clear();
    b.setRolloverIcon(icon2);
    harness.check(events.size(), 0);
    
    // set to null
    b.setRolloverIcon(null);
    harness.check(b.getRolloverIcon(), null);
    harness.check(b.isRolloverEnabled(), true);
    harness.check(events.size(), 1);
    e1 = (PropertyChangeEvent) events.get(0);
    harness.check(e1.getSource(), b);
    harness.check(e1.getPropertyName(), "rolloverIcon");
    harness.check(e1.getOldValue(), icon2);
    harness.check(e1.getNewValue(), null);
  }
}
