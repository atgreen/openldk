// Tags: JDK1.2

// Copyright (C) 2004 David Gilbert <david.gilbert@object-refinery.com>

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
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.javax.swing.SwingUtilities;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Rectangle;

import javax.swing.SwingUtilities;

/**
 * Some checks for the isRectangleContainingRectangle() method in the 
 * SwingUtilities class.
 */
public class isRectangleContainingRectangle implements Testlet {

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness) {
    
    Rectangle r0 = new Rectangle(0, 0, 0, 0);
    Rectangle r1 = new Rectangle(0, 0, 1, 1);
    Rectangle r2 = new Rectangle(1, 1, 1, 1);
    Rectangle r3 = new Rectangle(-1, -1, 2, 2);

    harness.check(SwingUtilities.isRectangleContainingRectangle(r0, r0));
    harness.check(!SwingUtilities.isRectangleContainingRectangle(r0, r1));
    harness.check(SwingUtilities.isRectangleContainingRectangle(r1, r0));
    harness.check(SwingUtilities.isRectangleContainingRectangle(r1, r1));
    harness.check(!SwingUtilities.isRectangleContainingRectangle(r1, r2));
    harness.check(!SwingUtilities.isRectangleContainingRectangle(r2, r0));
    harness.check(!SwingUtilities.isRectangleContainingRectangle(r2, r1));
    harness.check(SwingUtilities.isRectangleContainingRectangle(r2, r2));
    harness.check(SwingUtilities.isRectangleContainingRectangle(r3, r0));
    harness.check(SwingUtilities.isRectangleContainingRectangle(r3, r1));
    harness.check(!SwingUtilities.isRectangleContainingRectangle(r3, r2));
      
    harness.checkPoint("Null arguments");
    // test null argument - the API spec doesn't specify what should
    // happen, but a NullPointerException is the usual result elsewhere
    try
    {
      /* Rectangle r2 = */ SwingUtilities.isRectangleContainingRectangle(null, new Rectangle());
      harness.check(false);
    }
    catch (NullPointerException e) 
    {
      harness.check(true);
    }

    try
    {
      /* Rectangle r2 = */ SwingUtilities.isRectangleContainingRectangle(new Rectangle(), null);
      harness.check(false);
    }
    catch (NullPointerException e) 
    {
      harness.check(true);
    }
  }
  
}