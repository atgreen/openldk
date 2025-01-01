/* setDesktopIcon.java -- some checks for the setDesktopIcon() method in the
       JInternalFrame class.
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

package gnu.testlet.javax.swing.JInternalFrame;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.JInternalFrame;
import javax.swing.JInternalFrame.JDesktopIcon;

public class setDesktopIcon implements Testlet, PropertyChangeListener
{
  PropertyChangeEvent lastEvent;
  
  public void propertyChange(PropertyChangeEvent event)
  {
    lastEvent = event;
  }
  
  public void test(TestHarness harness)
  {
    JInternalFrame f = new JInternalFrame("Title");
    f.addPropertyChangeListener(this);
    JDesktopIcon icon = new JDesktopIcon(f);
    f.setDesktopIcon(icon);
    harness.check(f.getDesktopIcon(), icon);
    harness.check(lastEvent.getPropertyName(), "desktopIcon");
    harness.check(lastEvent.getSource(), f);
    harness.check(lastEvent.getNewValue(), icon);
    harness.check(lastEvent.getOldValue() != null);
    
    lastEvent = null;
    f.setDesktopIcon(null);
    harness.check(f.getDesktopIcon(), null);
    harness.check(lastEvent.getPropertyName(), "desktopIcon");
    harness.check(lastEvent.getSource(), f);
    harness.check(lastEvent.getNewValue(), null);
    harness.check(lastEvent.getOldValue(), icon);
    
    JInternalFrame f2 = new JInternalFrame("F2");
    JDesktopIcon icon2 = new JDesktopIcon(f2);
    f.setDesktopIcon(icon);
    harness.check(icon2.getInternalFrame(), f2);
  }
}
