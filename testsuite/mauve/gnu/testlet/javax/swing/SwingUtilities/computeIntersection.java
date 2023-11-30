// Tags: JDK1.2

// Copyright (C) 2004, 2006, David Gilbert <david.gilbert@object-refinery.com>

// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.

// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with GNU Classpath; see the file COPYING.  If not, write to the
// Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
// 02110-1301 USA.

package gnu.testlet.javax.swing.SwingUtilities;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Rectangle;

import javax.swing.SwingUtilities;

/**
 * Some checks for the computeIntersection() method in the SwingUtilities
 * class.
 */
public class computeIntersection implements Testlet {

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness) {
    
    Rectangle dest = new Rectangle(1, 1, 3, 3);
    
    harness.checkPoint("No intersection");
    
    // no intersection - top left 
    SwingUtilities.computeIntersection(0, 4, 1, 1, dest);
    harness.check(dest.isEmpty());

    // no intersection - top
    dest = new Rectangle(1, 1, 3, 3);
    SwingUtilities.computeIntersection(2, 4, 1, 1, dest);
    harness.check(dest.isEmpty());
    
    // no intersection - top right
    dest = new Rectangle(1, 1, 3, 3);
    SwingUtilities.computeIntersection(4, 4, 1, 1, dest);
    harness.check(dest.isEmpty());

    // no intersection - bottom left
    dest = new Rectangle(1, 1, 3, 3);    
    SwingUtilities.computeIntersection(0, 0, 1, 1, dest);
    harness.check(dest.isEmpty());

    // no intersection - bottom 
    dest = new Rectangle(1, 1, 3, 3);
    SwingUtilities.computeIntersection(2, 0, 1, 1, dest);
    harness.check(dest.isEmpty());

    // no intersection - bottom right
    dest = new Rectangle(1, 1, 3, 3);
    SwingUtilities.computeIntersection(4, 0, 1, 1, dest);
    harness.check(dest.isEmpty());
    
    // no intersection - left
    dest = new Rectangle(1, 1, 3, 3);
    SwingUtilities.computeIntersection(0, 2, 1, 1, dest);
    harness.check(dest.isEmpty());
    
    // no intersection - right
    dest = new Rectangle(1, 1, 3, 3);
    SwingUtilities.computeIntersection(4, 2, 1, 1, dest);
    harness.check(dest.isEmpty());
    
    // no intersection - empty rectangle
    dest = new Rectangle(1, 1, 3, 3);
    SwingUtilities.computeIntersection(2, 2, 0, 0, dest);
    harness.check(dest.isEmpty());
    
    harness.checkPoint("Intersection");

    // intersection - top left
    dest = new Rectangle(1, 1, 3, 3);
    SwingUtilities.computeIntersection(0, 3, 2, 2, dest);
    harness.check(dest, new Rectangle(1, 3, 1, 1));

    // intersection - top
    dest = new Rectangle(1, 1, 3, 3);
    SwingUtilities.computeIntersection(2, 3, 1, 2, dest);
    harness.check(dest, new Rectangle(2, 3, 1, 1));

    // intersection - top right
    dest = new Rectangle(1, 1, 3, 3);
    SwingUtilities.computeIntersection(3, 3, 2, 2, dest);
    harness.check(dest, new Rectangle(3, 3, 1, 1));

    // intersection - bottom left
    dest = new Rectangle(1, 1, 3, 3);
    SwingUtilities.computeIntersection(0, 0, 2, 2, dest);
    harness.check(dest, new Rectangle(1, 1, 1, 1));

    // intersection - bottom
    dest = new Rectangle(1, 1, 3, 3);
    SwingUtilities.computeIntersection(2, 0, 1, 2, dest);
    harness.check(dest, new Rectangle(2, 1, 1, 1));

    // intersection - bottom right
    dest = new Rectangle(1, 1, 3, 3);
    SwingUtilities.computeIntersection(3, 0, 2, 2, dest);
    harness.check(dest, new Rectangle(3, 1, 1, 1));

    // intersection - left
    dest = new Rectangle(1, 1, 3, 3);
    SwingUtilities.computeIntersection(0, 2, 2, 1, dest);
    harness.check(dest, new Rectangle(1, 2, 1, 1));
    
    // no intersection - right
    dest = new Rectangle(1, 1, 3, 3);
    SwingUtilities.computeIntersection(3, 2, 2, 1, dest);
    harness.check(dest, new Rectangle(3, 2, 1, 1));
    
    harness.checkPoint("Null arguments");
    // test null argument - the API spec doesn't specify what should
    // happen, but a NullPointerException is the usual result elsewhere
    try
    {
      /* Rectangle r2 = */ SwingUtilities.computeIntersection(1, 2, 3, 4, null);
      harness.check(false);
    }
    catch (NullPointerException e) 
    {
      harness.check(true);
    }

  }
  
}
