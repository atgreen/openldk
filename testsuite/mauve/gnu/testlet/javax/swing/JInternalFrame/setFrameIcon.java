/* setFrameIcon.java -- some checks for the setFrameIcon() method in the
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

import javax.swing.Icon;
import javax.swing.JInternalFrame;
import javax.swing.plaf.metal.MetalIconFactory;

public class setFrameIcon implements Testlet, PropertyChangeListener
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
    Icon icon = MetalIconFactory.getHorizontalSliderThumbIcon();
    f.setFrameIcon(icon);
    harness.check(f.getFrameIcon(), icon);
    harness.check(lastEvent.getPropertyName(), 
            JInternalFrame.FRAME_ICON_PROPERTY);
    harness.check(lastEvent.getSource(), f);
    harness.check(lastEvent.getNewValue(), icon);
    harness.check(lastEvent.getOldValue() != null);
    
    lastEvent = null;
    f.setFrameIcon(null);
    harness.check(f.getFrameIcon(), null);
    harness.check(lastEvent.getPropertyName(), 
            JInternalFrame.FRAME_ICON_PROPERTY);
    harness.check(lastEvent.getSource(), f);
    harness.check(lastEvent.getNewValue(), null);
    harness.check(lastEvent.getOldValue(), icon);
  }
}
