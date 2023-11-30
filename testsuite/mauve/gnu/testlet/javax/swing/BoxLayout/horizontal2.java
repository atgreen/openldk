// Tags: JDK1.2

// Copyright (C) 2005 Roman Kennke <roman@kennke.org>

// This file is part of Mauve.

// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.

// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

package gnu.testlet.javax.swing.BoxLayout;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.awt.Dimension;
import java.awt.Rectangle;

import javax.swing.BoxLayout;
import javax.swing.JComponent;

/**
 * Checks how space is distributed between components with different
 * preferred sizes.
 *
 * @author Roman Kennke
 */
public class horizontal2 implements Testlet
{
  public void test(TestHarness h)
  {
    JComponent comp = new JComponent(){};
    BoxLayout layout = new BoxLayout(comp, BoxLayout.X_AXIS);

    // add two components should equally share the available space between
    // them
    JComponent c1 = new JComponent(){};
    c1.setPreferredSize(new Dimension(50, 400));
    JComponent c2 = new JComponent(){};
    c2.setPreferredSize(new Dimension(150, 200));
    
    comp.add(c1);
    comp.add(c2);

    comp.setSize(400, 400);
    layout.layoutContainer(comp);

    Rectangle b1 = c1.getBounds();
    Rectangle b2 = c2.getBounds();
    h.check(b1.x, 0, String.valueOf(b1.x));
    h.check(b1.y, 0, String.valueOf(b1.y));
    h.check(b1.width, 150, String.valueOf(b1.width));
    h.check(b1.height, 400, String.valueOf(b1.height));
    h.check(b2.x, 150, String.valueOf(b2.x));
    h.check(b2.y, 0, String.valueOf(b2.y));
    h.check(b2.width, 249, String.valueOf(b2.width));
    h.check(b2.height, 400, String.valueOf(b2.height));
    
  }

}
