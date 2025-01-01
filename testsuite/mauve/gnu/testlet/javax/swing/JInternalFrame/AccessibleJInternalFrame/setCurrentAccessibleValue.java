/* setCurrentAccessibleValue.java -- some checks for the 
       setCurrentAccessibleValue() method in the AccessibleJInternalFrame class.
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

package gnu.testlet.javax.swing.JInternalFrame.AccessibleJInternalFrame;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleValue;
import javax.swing.JInternalFrame;
import javax.swing.JLayeredPane;
import javax.swing.JInternalFrame.JDesktopIcon;

public class setCurrentAccessibleValue 
  implements Testlet, PropertyChangeListener
{
  PropertyChangeEvent lastEvent;
  
  public void propertyChange(PropertyChangeEvent event)
  {
    lastEvent = event;
  }
  
  public void test(TestHarness harness)
  {
    JInternalFrame f = new JInternalFrame("Title");
    JDesktopIcon icon = f.getDesktopIcon();
    AccessibleContext ac = icon.getAccessibleContext();
    AccessibleValue av = ac.getAccessibleValue();
    
    // by trial and error, I determined that the "value" is the frame's layer
    harness.check(av.getCurrentAccessibleValue(), JLayeredPane.DEFAULT_LAYER);
    ac.addPropertyChangeListener(this);
    av.setCurrentAccessibleValue(JLayeredPane.PALETTE_LAYER);
    harness.check(f.getLayer(), JLayeredPane.PALETTE_LAYER.intValue());
    harness.check(lastEvent, null);  // no event is generated
    
    boolean set = av.setCurrentAccessibleValue(null);
    harness.check(!set);
    harness.check(f.getLayer(), JLayeredPane.PALETTE_LAYER.intValue());
  }
}
