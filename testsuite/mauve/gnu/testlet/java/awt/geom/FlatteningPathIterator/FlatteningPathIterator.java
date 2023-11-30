// Tags: JDK1.2

// Copyright (C) 2003 Sascha Brawer <brawer@dandelis.ch>

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
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.awt.geom.FlatteningPathIterator;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.awt.geom.Line2D;


/**
 * Checks whether the FlatteningPathIterator constructors work.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class FlatteningPathIterator
  implements Testlet
{
  public void test(TestHarness harness)
  {
    java.awt.geom.FlatteningPathIterator fpi;
    Throwable caught;

    // Checks 1 and 2: Three-argument form.
    fpi = new java.awt.geom.FlatteningPathIterator(
      new Line2D.Float().getPathIterator(null),
      0, 0);
    harness.check(fpi.getFlatness(), 0);          // 1
    harness.check(fpi.getRecursionLimit(), 0);    // 2

    // Check 3: Two-argument form.
    fpi = new java.awt.geom.FlatteningPathIterator(
      new Line2D.Float().getPathIterator(null),
      0);
    harness.check(fpi.getFlatness(), 0);          // 3

    // Check 4: Exception for flatness < 0.
    caught = null;
    try
    {
      fpi = new java.awt.geom.FlatteningPathIterator(
        new Line2D.Float().getPathIterator(null), -0.1, 4);
    }
    catch (Exception ex)
    {
      caught = ex;
    }
    harness.check(caught instanceof IllegalArgumentException);

    // Check 5: Exception for recursion limit < 0.
    caught = null;
    try
    {
      fpi = new java.awt.geom.FlatteningPathIterator(
        new Line2D.Float().getPathIterator(null), 10, -1);
    }
    catch (Exception ex)
    {
      caught = ex;
    }
    harness.check(caught instanceof IllegalArgumentException);
  }
}
