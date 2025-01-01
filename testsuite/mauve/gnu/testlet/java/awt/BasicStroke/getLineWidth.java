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

package gnu.testlet.java.awt.BasicStroke;

import java.awt.BasicStroke;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Test the method BasicStroke.getLineWidth()
 */
public class getLineWidth implements Testlet
{
  /**
   * Runs the test using the specified harness. 
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)
  {
    positiveTests(harness);
    negativeTests(harness);
  }

  /**
   * Positive tests for the method BasicStroke.getWidth().
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void positiveTests(TestHarness harness)
  {
    harness.checkPoint("positive tests");

    // tested object
    BasicStroke stroke;

    // default value
    stroke = new BasicStroke();
    harness.check(stroke.getLineWidth(), 1.0f);

    // zero stroke width should be fine
    stroke = new BasicStroke(0.0f);
    harness.check(stroke.getLineWidth(), 0.0f);

    // positive stroke width should be fine
    stroke = new BasicStroke(10.0f);
    harness.check(stroke.getLineWidth(), 10.0f);

    // constructor(width, endCap, lineJoin)
    stroke = new BasicStroke(10.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND);
    harness.check(stroke.getLineWidth(), 10.0f);

    // constructor(width, endCap, lineJoin, mitterLimit)
    stroke = new BasicStroke(10.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND, 1.0f);
    harness.check(stroke.getLineWidth(), 10.0f);
  }

  /**
   * Negative tests for the method BasicStroke.getWidth().
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void negativeTests(TestHarness harness)
  {
    harness.checkPoint("positive tests");

    // tested object
    BasicStroke stroke;

    // negative width value
    // constructor(width)
    try {
      stroke = new BasicStroke(-10.0f);
      // should not happen
      harness.check(false);
    }
    catch (IllegalArgumentException e) {
      harness.check(true);
    }

    // negative width value
    // constructor(width, endCap, lineJoin)
    try {
      stroke = new BasicStroke(-10.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND);
      // should not happen
      harness.check(false);
    }
    catch (IllegalArgumentException e) {
      harness.check(true);
    }

    // negative width value
    // constructor(width, endCap, lineJoin, mitterLimit)
    try {
      stroke = new BasicStroke(-10.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND, 1.0f);
      // should not happen
      harness.check(false);
    }
    catch (IllegalArgumentException e) {
      harness.check(true);
    }
  }

}

