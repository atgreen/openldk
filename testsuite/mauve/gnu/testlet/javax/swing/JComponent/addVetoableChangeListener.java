/* addVetoableChangeListener.java -- some checks for the 
       addVetoableChangeListener() method in the JComponent class.
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

package gnu.testlet.javax.swing.JComponent;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyVetoException;
import java.beans.VetoableChangeListener;

import javax.swing.JButton;
import javax.swing.JComponent;

public class addVetoableChangeListener 
  implements Testlet, VetoableChangeListener
{
  public void vetoableChange(PropertyChangeEvent e) 
    throws PropertyVetoException 
  {
  }

public void test(TestHarness harness)
  {
    JComponent c = new JButton("ABC");
    harness.check(c.getVetoableChangeListeners().length, 0);
    c.addVetoableChangeListener(this);
    harness.check(c.getVetoableChangeListeners().length, 1);
    c.addVetoableChangeListener(null);
    harness.check(c.getVetoableChangeListeners().length, 1);
  }
}
