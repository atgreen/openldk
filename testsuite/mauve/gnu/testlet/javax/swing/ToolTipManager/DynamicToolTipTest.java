/* DynamicToolTipTest.java -- Tests dynamically updated tooltips
   Copyright (C) 2006 Roman Kennke (kennke@aicas.com)
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

package gnu.testlet.javax.swing.ToolTipManager;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.MouseEvent;

import javax.swing.JPanel;
import javax.swing.ToolTipManager;

import gnu.testlet.VisualTestlet;

/**
 * Tests if dynamic tooltips {@link JComponent.getToolTipText(MouseEvent)}
 * work correctly.
 *
 * This is a regression test for bug:
 * http://gcc.gnu.org/bugzilla/show_bug.cgi?id=27957
 */
public class DynamicToolTipTest extends VisualTestlet
{

  public String getInstructions()
  {
    return "Move the mouse pointer over the empty panel. Wait for 1-2 seconds"
           + " until the tooltip appears. Moving the mouse should update "
           + "the tooltip text with the current mouse coordinates.";
  }

  public String getExpectedResults()
  {
    return "A tooltip should appear after 1-2 seconds. When the mouse is moved"
           + " further, the tooltip text is updated with the mouse coordinates"
           + " and the tooltip location follows the mouse pointer";
  }

  public Component getTestComponent()
  {
    JPanel p = new JPanel()
    {
      public Dimension getPreferredSize()
      {
        return new Dimension(200, 200);
      }
      public String getToolTipText(MouseEvent ev)
      {
        return "" + ev.getX() + ", " + ev.getY(); 
      }
    };
    ToolTipManager.sharedInstance().registerComponent(p);
    return p;
  }

}
