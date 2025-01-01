// Tags: JDK1.5

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
 * Test the method BasicStroke.getLineJoin()
 */
public class getLineJoin implements Testlet
{
  /**
   * Default value for MITER_LIMIT
   */
  private static final float MITER_LIMIT = 10.0f;

  /**
   * Default value for DASH
   */
  private static final float[] DASH_ARRAY = {1.0f, 2.0f};

  /**
   * Default value for DASH_PHASE
   */
  private static final float DASH_PHASE = 0.0f;

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
   * Positive tests for the method BasicStroke.getLineJoin().
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
    harness.check(stroke.getLineJoin(), BasicStroke.JOIN_MITER);

    // constructor(width, endCap, lineJoin)
    stroke = new BasicStroke(1.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_BEVEL);
    harness.check(stroke.getLineJoin(), BasicStroke.JOIN_BEVEL);
    stroke = new BasicStroke(1.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_MITER);
    harness.check(stroke.getLineJoin(), BasicStroke.JOIN_MITER);
    stroke = new BasicStroke(1.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND);
    harness.check(stroke.getLineJoin(), BasicStroke.JOIN_ROUND);

    // constructor(width, endCap, lineJoin, miterLimit)
    stroke = new BasicStroke(1.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_BEVEL, 1.0f);
    harness.check(stroke.getLineJoin(), BasicStroke.JOIN_BEVEL);
    stroke = new BasicStroke(1.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_MITER, 1.0f);
    harness.check(stroke.getLineJoin(), BasicStroke.JOIN_MITER);
    stroke = new BasicStroke(1.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND, 1.0f);
    harness.check(stroke.getLineJoin(), BasicStroke.JOIN_ROUND);

    // constructor(width, endCap, lineJoin, miterLimit, dash[], dashPhase)
    stroke = new BasicStroke(1.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_BEVEL, MITER_LIMIT, DASH_ARRAY, DASH_PHASE);
    harness.check(stroke.getLineJoin(), BasicStroke.JOIN_BEVEL);
    stroke = new BasicStroke(1.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_MITER, MITER_LIMIT, DASH_ARRAY, DASH_PHASE);
    harness.check(stroke.getLineJoin(), BasicStroke.JOIN_MITER);
    stroke = new BasicStroke(1.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND, MITER_LIMIT, DASH_ARRAY, DASH_PHASE);
    harness.check(stroke.getLineJoin(), BasicStroke.JOIN_ROUND);
  }

  /**
   * Negative tests for the method BasicStroke.getLineJoin().
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void negativeTests(TestHarness harness)
  {
    harness.checkPoint("positive tests");

    // tested object
    @SuppressWarnings("unused")
    BasicStroke stroke;

    // join code is outside the range 0..2
    // constructor(width, endCap, lineJoin)
    try {
      stroke = new BasicStroke(1.0f, BasicStroke.CAP_ROUND, 10);
      // should not happen
      harness.check(false);
    }
    catch (IllegalArgumentException e) {
      harness.check(true);
    }

    // join code is outside the range 0..2
    // constructor(width, endCap, lineJoin)
    try {
      stroke = new BasicStroke(1.0f, BasicStroke.CAP_ROUND, -10);
      // should not happen
      harness.check(false);
    }
    catch (IllegalArgumentException e) {
      harness.check(true);
    }

    // special values for join code
    // constructor(width, endCap, lineJoin)
    try {
      stroke = new BasicStroke(1.0f, BasicStroke.CAP_ROUND, Integer.MAX_VALUE);
      // should not happen
      harness.check(false);
    }
    catch (IllegalArgumentException e) {
      harness.check(true);
    }

    // special values for join code
    // constructor(width, endCap, lineJoin)
    try {
      stroke = new BasicStroke(1.0f, BasicStroke.CAP_ROUND, Integer.MIN_VALUE);
      // should not happen
      harness.check(false);
    }
    catch (IllegalArgumentException e) {
      harness.check(true);
    }

    // join code is outside the range 0..2
    // constructor(width, endCap, lineJoin, miterLimit)
    try {
      stroke = new BasicStroke(1.0f, BasicStroke.CAP_ROUND, 10, MITER_LIMIT);
      // should not happen
      harness.check(false);
    }
    catch (IllegalArgumentException e) {
      harness.check(true);
    }

    // join code is outside the range 0..2
    // constructor(width, endCap, lineJoin, miterLimit)
    try {
      stroke = new BasicStroke(1.0f, BasicStroke.CAP_ROUND, -10, MITER_LIMIT);
      // should not happen
      harness.check(false);
    }
    catch (IllegalArgumentException e) {
      harness.check(true);
    }

    // join code is outside the range 0..2
    // constructor(width, endCap, lineJoin, miterLimit, dash[], dashPhase)
    try {
      stroke = new BasicStroke(1.0f, BasicStroke.CAP_ROUND, 10, MITER_LIMIT, DASH_ARRAY, DASH_PHASE);
      // should not happen
      harness.check(false);
    }
    catch (IllegalArgumentException e) {
      harness.check(true);
    }

    // join code is outside the range 0..2
    // constructor(width, endCap, lineJoin, dash[], miterLimit)
    try {
      stroke = new BasicStroke(1.0f, BasicStroke.CAP_ROUND, -10, MITER_LIMIT, DASH_ARRAY, DASH_PHASE);
      // should not happen
      harness.check(false);
    }
    catch (IllegalArgumentException e) {
      harness.check(true);
    }
  }

}

