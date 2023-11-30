/* getMinimumSpan.java -- Tests the minimumSpan default implementation
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
// Uses: TestView

package gnu.testlet.javax.swing.text.View;

import javax.swing.text.View;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Tests the getMinimumSpan() default implementation.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class getMinimumSpan implements Testlet
{

  /**
   * A subclass of TestView that allows to customize the resizeWeight.
   */
  private class PositiveResizeWeightView extends TestView {

    int resizeWeight = 0;

    /**
     * Overridden to return a custom resize weight.
     *
     * @return resizeWeight
     */
    public int getResizeWeight(int axis) {
      return resizeWeight;
    }
  }

  /**
   * The entry point into this test.
   *
   * @param harness the test harness to use
   */
  public void test(TestHarness harness)
  {
    testXAxis(harness);
    testYAxis(harness);
    testPositiveResizeWeight(harness);
  }

  /**
   * Tests the default impl for the X_AXIS. This will forward to
   * getPreferredSpan().
   *
   * @param harness the test harness to use
   */
  private void testXAxis(TestHarness harness)
  {
    TestView v = new TestView();
    v.preferred[View.X_AXIS] = 123F;
    harness.check(v.getMinimumSpan(View.X_AXIS), 123F);
  }

  /**
   * Tests the default impl for the Y_AXIS. This will forward to
   * getPreferredSpan().
   *
   * @param harness the test harness to use
   */
  private void testYAxis(TestHarness harness)
  {
    TestView v = new TestView();
    v.preferred[View.Y_AXIS] = 123F;
    harness.check(v.getMinimumSpan(View.Y_AXIS), 123F);
  }

  /**
   * Tests the minimumSpan with a positive resizeWeight.
   *
   * @param harness the test harness to use
   */
  private void testPositiveResizeWeight(TestHarness harness)
  {
    PositiveResizeWeightView v = new PositiveResizeWeightView();
    v.resizeWeight = 100;
    v.preferred[View.X_AXIS] = 123F;
    v.preferred[View.Y_AXIS] = 123F;
    harness.check((int) v.getMinimumSpan(View.X_AXIS), 0);
    harness.check((int) v.getMinimumSpan(View.Y_AXIS), 0);
  }

}
