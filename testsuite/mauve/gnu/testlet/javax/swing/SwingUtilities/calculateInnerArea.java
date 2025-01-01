/* calculateInnerArea.java -- some checks for the calculateInnerArea() method
                              in the SwingUtilities class.
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

package gnu.testlet.javax.swing.SwingUtilities;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Dimension;
import java.awt.Rectangle;

import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.border.EmptyBorder;

public class calculateInnerArea implements Testlet
{
  public void test(TestHarness harness)
  {
    JPanel c = new JPanel();
    c.setBorder(new EmptyBorder(1, 2, 3, 4));
    c.setSize(new Dimension(100, 200));
    
    // test with null r
    Rectangle r = SwingUtilities.calculateInnerArea(c, null);
    harness.check(r, new Rectangle(2, 1, 94, 196));
   
    // check that passed in rectangle is returned
    Rectangle rr = new Rectangle();
    r = SwingUtilities.calculateInnerArea(c, rr);
    harness.check(r == rr);
    harness.check(r, new Rectangle(2, 1, 94, 196));
    
    // try null component
    rr = SwingUtilities.calculateInnerArea(null, r);
    harness.check(rr, null);
    harness.check(r, new Rectangle(2, 1, 94, 196));
  }
}
