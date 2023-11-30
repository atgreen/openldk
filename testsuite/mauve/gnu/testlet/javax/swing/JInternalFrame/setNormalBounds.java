/* setNormalBounds.java -- some checks for the setNormalBounds() method
       in the JInternalFrame class.
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

import java.awt.Rectangle;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.JDesktopPane;
import javax.swing.JInternalFrame;

public class setNormalBounds implements Testlet, PropertyChangeListener
{
  PropertyChangeEvent lastEvent;
  
  public void propertyChange(PropertyChangeEvent event)
  {
    lastEvent = event;
  }
  public void test(TestHarness harness)
  {
    JDesktopPane desktop = new JDesktopPane();
    JInternalFrame f = new JInternalFrame("Title");
    desktop.add(f);
    f.addPropertyChangeListener(this);
    harness.check(f.getNormalBounds(), new Rectangle());
    f.setBounds(4, 3, 2, 1);
    harness.check(f.getNormalBounds(), new Rectangle(4, 3, 2, 1));
    
    Rectangle normalBounds = new Rectangle(1, 2, 3, 4);
    f.setNormalBounds(normalBounds);
    harness.check(f.getNormalBounds(), new Rectangle(1, 2, 3, 4));
    harness.check(f.getNormalBounds() == normalBounds);
    harness.check(f.getBounds(), new Rectangle(4, 3, 2, 1));
    harness.check(lastEvent, null);
    
    // after setting to null, getNormalBounds() reverts to the component bounds
    f.setNormalBounds(null);
    harness.check(f.getNormalBounds(), new Rectangle(4, 3, 2, 1));
  }
}
