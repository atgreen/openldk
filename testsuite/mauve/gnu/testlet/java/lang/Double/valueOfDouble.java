// Test of Double method valueOf(double).

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

package gnu.testlet.java.lang.Double;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Some checks for the valueOf(double) method in the {@link Double} class.
 */
public class valueOfDouble implements Testlet 
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
    harness.check(Double.valueOf(1.0), new Double(1.0));
    harness.check(Double.valueOf(+1.0), new Double(1.0));
    harness.check(Double.valueOf(-1.0), new Double(-1.0));

    harness.check(Double.valueOf(42.), new Double(42.0));
    harness.check(Double.valueOf(.42), new Double(0.42));
    harness.check(Double.valueOf(Double.MIN_VALUE), new Double(Double.MIN_VALUE));
    harness.check(Double.valueOf(Double.MAX_VALUE), new Double(Double.MAX_VALUE));
  }

  /**
   * Some checks for values that should be read as Double.POSITIVE_INFINITY
   * or Double.NEGATIVE_INFINITY.
   * 
   * @param harness  the test harness.
   */
  public void testInfinities(TestHarness harness) 
  {
    harness.check(Double.valueOf(Double.POSITIVE_INFINITY), new Double(Double.POSITIVE_INFINITY));
    harness.check(Double.valueOf(Double.NEGATIVE_INFINITY), new Double(Double.NEGATIVE_INFINITY));
  }

  /**
   * Some checks for 'NaN' values.
   * 
   * @param harness  the test harness.
   */
  public void testNaN(TestHarness harness) 
  {
    harness.check(Double.valueOf(Double.NaN), new Double(Double.NaN));
    harness.check(Double.valueOf(-Double.NaN), new Double(Double.NaN));
    harness.check(Double.valueOf(+Double.NaN), new Double(Double.NaN));
  }
}

