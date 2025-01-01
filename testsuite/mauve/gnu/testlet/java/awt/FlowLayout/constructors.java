// Tags: JDK1.4

// Copyright (C) 2011 Pavel Tisnovsky <ptisnovs@redhat.com>

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
// the Free Software Foundation, Inc., 51 Franklin Street,
// Fifth Floor, Boston, MA 02110-1301 USA.

package gnu.testlet.java.awt.FlowLayout;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.FlowLayout;

/**
 * Tests which checks constructors in the {@link FlowLayout} class.  
 */
public class constructors implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)
  {
    testConstructorNoParams(harness);
    testConstructorOneParam(harness);
    testConstructorThreeParams(harness);
    testConstructorThreeParamsNegativeGaps(harness);
  }

  private void doCheck(TestHarness harness, FlowLayout flowLayout, int alignment, int hgap, int vgap)
  {
    harness.check(flowLayout.getAlignment(), alignment);
    harness.check(flowLayout.getHgap(), hgap);
    harness.check(flowLayout.getVgap(), vgap);
  }

  /**
   * Test layout manager constructed using constructor without any parameter.
   *
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  private void testConstructorNoParams(TestHarness harness)
  {
    harness.checkPoint("()");
    FlowLayout flowLayout1 = new FlowLayout();
    doCheck(harness, flowLayout1, FlowLayout.CENTER, 5, 5);
  }

  /**
   * Test layout manager constructed using constructor with one parameter.
   *
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  private void testConstructorOneParam(TestHarness harness)
  {
    harness.checkPoint("(int align)");
    FlowLayout flowLayout2 = new FlowLayout(FlowLayout.CENTER);
    doCheck(harness, flowLayout2, FlowLayout.CENTER, 5, 5);

    flowLayout2 = new FlowLayout(FlowLayout.LEFT);
    doCheck(harness, flowLayout2, FlowLayout.LEFT, 5, 5);

    flowLayout2 = new FlowLayout(FlowLayout.RIGHT);
    harness.check(flowLayout2.getAlignment(), FlowLayout.RIGHT);
    doCheck(harness, flowLayout2, FlowLayout.RIGHT, 5, 5);
  }

  /**
   * Test layout manager constructed using constructor with three parameters.
   *
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  private void testConstructorThreeParams(TestHarness harness)
  {
    harness.checkPoint("(int align, int hgap, int vgap)");
    FlowLayout flowLayout3 = new FlowLayout(FlowLayout.CENTER, 50, 50);
    doCheck(harness, flowLayout3, FlowLayout.CENTER, 50, 50);
  }

  /**
   * Test layout manager constructed using constructor with three parameters.
   *
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  private void testConstructorThreeParamsNegativeGaps(TestHarness harness)
  {
    harness.checkPoint("(int align, int -hgap, int -vgap)");
    FlowLayout flowLayout4 = new FlowLayout(FlowLayout.CENTER, -50, 50);
    doCheck(harness, flowLayout4, FlowLayout.CENTER, -50, 50);

    flowLayout4 = new FlowLayout(FlowLayout.CENTER, 50, -50);
    doCheck(harness, flowLayout4, FlowLayout.CENTER, 50, -50);

    flowLayout4 = new FlowLayout(FlowLayout.CENTER, -50, -50);
    doCheck(harness, flowLayout4, FlowLayout.CENTER, -50, -50);
  }

}
