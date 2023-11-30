// Test of Float method valueOf(float).

// Copyright 2012 Red Hat, Inc.
// Written by Pavel Tisnovsky <ptisnovs@redhat.com>

// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published 
// by the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software Foundation
// Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307 USA

// Tags: JDK1.4
// Tags: CompileOptions: -source 1.4 -target 1.4

package gnu.testlet.java.lang.Float;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Some checks for the valueOf(float) method in the {@link Float} class.
 */
public class valueOfFloat implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    testRegular(harness);
    testInfinities(harness);
    testNaN(harness);
  }

  /**
   * Tests some regular values.
   * 
   * @param harness  the test harness.
   */
  public void testRegular(TestHarness harness) 
  {
    harness.check(Float.valueOf(1.0f), new Float(1.0));
    harness.check(Float.valueOf(+1.0f), new Float(1.0));
    harness.check(Float.valueOf(-1.0f), new Float(-1.0));

    harness.check(Float.valueOf(42.f), new Float(42.0));
    harness.check(Float.valueOf(.42f), new Float(0.42));
    harness.check(Float.valueOf(Float.MIN_VALUE), new Float(Float.MIN_VALUE));
    harness.check(Float.valueOf(Float.MAX_VALUE), new Float(Float.MAX_VALUE));
  }

  /**
   * Some checks for values that should be read as Float.POSITIVE_INFINITY
   * or Float.NEGATIVE_INFINITY.
   * 
   * @param harness  the test harness.
   */
  public void testInfinities(TestHarness harness) 
  {
    harness.check(Float.valueOf(Float.POSITIVE_INFINITY), new Float(Float.POSITIVE_INFINITY));
    harness.check(Float.valueOf(Float.NEGATIVE_INFINITY), new Float(Float.NEGATIVE_INFINITY));
  }

  /**
   * Some checks for 'NaN' values.
   * 
   * @param harness  the test harness.
   */
  public void testNaN(TestHarness harness) 
  {
    harness.check(Float.valueOf(Float.NaN), new Float(Float.NaN));
    harness.check(Float.valueOf(-Float.NaN), new Float(Float.NaN));
    harness.check(Float.valueOf(+Float.NaN), new Float(Float.NaN));
  }
}

