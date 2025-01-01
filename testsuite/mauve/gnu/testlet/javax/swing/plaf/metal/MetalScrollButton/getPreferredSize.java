// Tags: JDK1.2

// Copyright (C) 2005 David Gilbert <david.gilbert@object-refinery.com>

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
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
// Boston, MA 02110-1301 USA.

package gnu.testlet.javax.swing.plaf.metal.MetalScrollButton;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Dimension;

import javax.swing.plaf.metal.MetalScrollButton;

/**
 * Some checks for the getPreferredSize() method in the 
 * {@link MetalScrollButton} class.  
 */
public class getPreferredSize implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)  
  {
    MetalScrollButton b = new MetalScrollButton(MetalScrollButton.NORTH, 27, true);
    harness.check(b.getPreferredSize(), new Dimension(27, 25));

    b = new MetalScrollButton(MetalScrollButton.NORTH, 27, false);
    harness.check(b.getPreferredSize(), new Dimension(27, 25));

    b = new MetalScrollButton(MetalScrollButton.NORTH, 15, true);
    harness.check(b.getPreferredSize(), new Dimension(15, 13));
  
    b = new MetalScrollButton(MetalScrollButton.NORTH, 15, false);
    harness.check(b.getPreferredSize(), new Dimension(15, 13));
  
    b = new MetalScrollButton(MetalScrollButton.EAST, 27, true);
    harness.check(b.getPreferredSize(), new Dimension(26, 27));

    b = new MetalScrollButton(MetalScrollButton.EAST, 27, false);
    harness.check(b.getPreferredSize(), new Dimension(25, 27));

    b = new MetalScrollButton(MetalScrollButton.EAST, 15, true);
    harness.check(b.getPreferredSize(), new Dimension(14, 15));
  
    b = new MetalScrollButton(MetalScrollButton.EAST, 15, false);
    harness.check(b.getPreferredSize(), new Dimension(13, 15));

    b = new MetalScrollButton(MetalScrollButton.SOUTH, 27, true);
    harness.check(b.getPreferredSize(), new Dimension(27, 26));

    b = new MetalScrollButton(MetalScrollButton.SOUTH, 27, false);
    harness.check(b.getPreferredSize(), new Dimension(27, 25));

    b = new MetalScrollButton(MetalScrollButton.SOUTH, 15, true);
    harness.check(b.getPreferredSize(), new Dimension(15, 14));
  
    b = new MetalScrollButton(MetalScrollButton.SOUTH, 15, false);
    harness.check(b.getPreferredSize(), new Dimension(15, 13));

    b = new MetalScrollButton(MetalScrollButton.WEST, 27, true);
    harness.check(b.getPreferredSize(), new Dimension(25, 27));

    b = new MetalScrollButton(MetalScrollButton.WEST, 27, false);
    harness.check(b.getPreferredSize(), new Dimension(25, 27));

    b = new MetalScrollButton(MetalScrollButton.WEST, 15, true);
    harness.check(b.getPreferredSize(), new Dimension(13, 15));
  
    b = new MetalScrollButton(MetalScrollButton.WEST, 15, false);
    harness.check(b.getPreferredSize(), new Dimension(13, 15));
  
  }

}
