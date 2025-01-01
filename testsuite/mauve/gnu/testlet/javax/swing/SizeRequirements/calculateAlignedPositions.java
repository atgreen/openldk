// Tags: JDK1.4

// Copyright (C) 2005 Roman Kennke <kennke@aicas.com>

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

package gnu.testlet.javax.swing.SizeRequirements;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.SizeRequirements;

/**
 * Tests if the calculateAlignedPositions method works correctly.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class calculateAlignedPositions implements Testlet
{

  /**
   * Starts the test run.
   *
   * @param harness the test harness to use
   */
  public void test(TestHarness harness)
  {
    test1(harness);
  }

  /**
   * This checks the method with two elements and values that I found in a bug
   * with BoxLayout. Kind of a corner case.
   *
   * @param harness the test harness to use
   */
  private void test1(TestHarness harness)
  {
    SizeRequirements req1 = new SizeRequirements(282, 282, 282, 0.5F);
    SizeRequirements req2 = new SizeRequirements(599, 599, 2147483647, 0.5F);
    SizeRequirements[] els = new SizeRequirements[]{req1, req2};
    // Only the alignment field of total is used.
    SizeRequirements total = SizeRequirements.getAlignedSizeRequirements(els);
    int[] offsets = new int[2];
    int[] spans = new int[2];
    SizeRequirements.calculateAlignedPositions(112, total, els, offsets, spans);
    harness.check(offsets[0], 0);
    harness.check(offsets[1], 0);
    harness.check(spans[0], 112);
    harness.check(spans[1], 112);
  }
}
