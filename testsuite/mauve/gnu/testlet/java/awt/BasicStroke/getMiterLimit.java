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
 * Test the method BasicStroke.getMitterLimit()
 */
public class getMiterLimit implements Testlet
{
  /**
   * Default value for MITER_LIMIT
   */
  private static final float DEFAULT_MITER_LIMIT = 10.0f;

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
    positiveTestsSmallMitterOtherJoinTypes(harness, BasicStroke.JOIN_ROUND);
    positiveTestsSmallMitterOtherJoinTypes(harness, BasicStroke.JOIN_BEVEL);
    negativeTests(harness);
  }

  /**
   * Positive tests for the method BasicStroke.getMitterLimit().
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void positiveTests(TestHarness harness)
  {
    harness.checkPoint("positive tests");

    BasicStroke stroke = null;

    // default value
    stroke = new BasicStroke();
    harness.check(stroke.getMiterLimit(), DEFAULT_MITER_LIMIT);

    // constructor(width, endCap, lineJoin)
    testMiterLimit(harness);

    // constructor(width, endCap, lineJoin, miterLimit = 1.0f)
    testMiterLimit(harness, 1.0f);

    // constructor(width, endCap, lineJoin, miterLimit = 10.0f)
    testMiterLimit(harness, 10.0f);

    // constructor(width, endCap, lineJoin, miterLimit = 10.0f)
    testMiterLimit(harness, Integer.MAX_VALUE);
  }

  /**
   * Another positive tests for the method BasicStroke.getMitterLimit().
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void positiveTestsSmallMitterOtherJoinTypes(TestHarness harness, int joinType)
  {
    harness.checkPoint("positive tests");

    // tested object
    BasicStroke stroke = null;

    // miter limit is below 1.0
    // constructor(width, endCap, lineJoin)
    try {
      stroke = new BasicStroke(1.0f, BasicStroke.CAP_ROUND, joinType, 0.0f);
      harness.check(true);
    }
    catch (IllegalArgumentException e) {
      // should not happen
      harness.check(false);
    }

    // miter limit is negative
    // constructor(width, endCap, lineJoin)
    try {
      stroke = new BasicStroke(1.0f, BasicStroke.CAP_ROUND, joinType, -1.0f);
      harness.check(true);
    }
    catch (IllegalArgumentException e) {
      // should not happen
      harness.check(false);
    }

    // miter limit is negative
    // constructor(width, endCap, lineJoin)
    try {
      stroke = new BasicStroke(1.0f, BasicStroke.CAP_ROUND, joinType, -1000.0f);
      harness.check(true);
    }
    catch (IllegalArgumentException e) {
      // should not happen
      harness.check(false);
    }

    // miter limit is negative
    // constructor(width, endCap, lineJoin)
    try {
      stroke = new BasicStroke(1.0f, BasicStroke.CAP_ROUND, joinType, Integer.MIN_VALUE);
      harness.check(true);
    }
    catch (IllegalArgumentException e) {
      // should not happen
      harness.check(false);
    }

    // constructor(width, endCap, lineJoin, miterLimit, dash[], dashPhase)
    try {
      stroke = new BasicStroke(1.0f, BasicStroke.CAP_ROUND, joinType, 0.0f, DASH_ARRAY, DASH_PHASE);
      harness.check(true);
    }
    catch (IllegalArgumentException e) {
      // should not happen
      harness.check(false);
    }

    // constructor(width, endCap, lineJoin, dash[], miterLimit)
    try {
      stroke = new BasicStroke(1.0f, BasicStroke.CAP_ROUND, joinType, -10.0f, DASH_ARRAY, DASH_PHASE);
      harness.check(true);
    }
    catch (IllegalArgumentException e) {
      // should not happen
      harness.check(false);
    }
  }


  /**
   * Negative tests for the method BasicStroke.getMitterLimit().
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void negativeTests(TestHarness harness)
  {
    harness.checkPoint("positive tests");

    // tested object
    BasicStroke stroke = null;

    // miter limit is below 1.0
    // constructor(width, endCap, lineJoin)
    try {
      stroke = new BasicStroke(1.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_MITER, 0.0f);
      // should not happen
      harness.check(false);
    }
    catch (IllegalArgumentException e) {
      harness.check(true);
    }

    // miter limit is negative
    // constructor(width, endCap, lineJoin)
    try {
      stroke = new BasicStroke(1.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_MITER, -1.0f);
      // should not happen
      harness.check(false);
    }
    catch (IllegalArgumentException e) {
      harness.check(true);
    }

    // miter limit is negative
    // constructor(width, endCap, lineJoin)
    try {
      stroke = new BasicStroke(1.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_MITER, -1000.0f);
      // should not happen
      harness.check(false);
    }
    catch (IllegalArgumentException e) {
      harness.check(true);
    }

    // miter limit is negative
    // constructor(width, endCap, lineJoin)
    try {
      stroke = new BasicStroke(1.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_MITER, Integer.MIN_VALUE);
      // should not happen
      harness.check(false);
    }
    catch (IllegalArgumentException e) {
      harness.check(true);
    }

    // constructor(width, endCap, lineJoin, miterLimit, dash[], dashPhase)
    try {
      stroke = new BasicStroke(1.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_MITER, 0.0f, DASH_ARRAY, DASH_PHASE);
      // should not happen
      harness.check(false);
    }
    catch (IllegalArgumentException e) {
      harness.check(true);
    }

    // constructor(width, endCap, lineJoin, dash[], miterLimit)
    try {
      stroke = new BasicStroke(1.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_MITER, -10.0f, DASH_ARRAY, DASH_PHASE);
      // should not happen
      harness.check(false);
    }
    catch (IllegalArgumentException e) {
      harness.check(true);
    }
  }

  /**
   * Tests for the method BasicStroke.getMitterLimit() for various caps and
   * joins settings.
   * 
   * @param harness
   *            the test harness (<code>null</code> not permitted).
   * @param miterLimit
   *            miter limit
   */
  private void testMiterLimit(TestHarness harness)
  {
    // tested object
    BasicStroke stroke;

    final int[] caps = {BasicStroke.CAP_BUTT, BasicStroke.CAP_ROUND, BasicStroke.CAP_SQUARE};
    final int[] joins = {BasicStroke.JOIN_BEVEL, BasicStroke.JOIN_MITER, BasicStroke.JOIN_ROUND};

    for (int cap = 0; cap < caps.length; cap++)
    {
      for (int join = 0; join < joins.length; join++)
      {
        stroke = new BasicStroke(1.0f, caps[cap], joins[join]);
        harness.check(stroke.getMiterLimit(), DEFAULT_MITER_LIMIT);
      }
    }
  }

  /**
   * Tests for the method BasicStroke.getMitterLimit() for various caps and
   * joins settings.
   * 
   * @param harness
   *            the test harness (<code>null</code> not permitted).
   * @param miterLimit
   *            miter limit
   */
  private void testMiterLimit(TestHarness harness, float miterLimit)
  {
    // tested object
    BasicStroke stroke;

    final int[] caps = {BasicStroke.CAP_BUTT, BasicStroke.CAP_ROUND, BasicStroke.CAP_SQUARE};
    final int[] joins = {BasicStroke.JOIN_BEVEL, BasicStroke.JOIN_MITER, BasicStroke.JOIN_ROUND};

    for (int cap = 0; cap < caps.length; cap++)
    {
      for (int join = 0; join < joins.length; join++)
      {
        stroke = new BasicStroke(1.0f, caps[cap], joins[join], miterLimit);
        harness.check(stroke.getMiterLimit(), miterLimit);
      }
    }
  }
}
