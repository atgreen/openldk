/* setResizable.java -- Checks the setResizable method
   Copyright (C) 2006 Roman Kennke (kennke@aicas.com)
       and David Gilbert <david.gilbert@object-refinery.com>
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

package gnu.testlet.javax.swing.JInternalFrame;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.JInternalFrame;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class setResizable implements Testlet, PropertyChangeListener
{
  PropertyChangeEvent lastEvent;
  
  public void propertyChange(PropertyChangeEvent e)
  {
    lastEvent = e;
  }
  
  public void test(TestHarness harness)
  {
    JInternalFrame f = new JInternalFrame("F1");
    harness.check(!f.isResizable());
    f.addPropertyChangeListener(this);
    f.setResizable(true);
    harness.check(f.isResizable());
    harness.check(lastEvent.getPropertyName(), "resizable");
    harness.check(lastEvent.getSource(), f);
    harness.check(lastEvent.getOldValue(), Boolean.FALSE);
    harness.check(lastEvent.getNewValue(), Boolean.TRUE);
  }
}
