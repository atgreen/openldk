// Tags: JDK1.3

//  Copyright (C) 2012 Pavel Tisnovsky <ptisnovs@redhat.com>

//  This file is part of Mauve.

//  Mauve is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2, or (at your option)
//  any later version.

//  Mauve is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.

//  You should have received a copy of the GNU General Public License
//  along with Mauve; see the file COPYING.  If not, write to
//  the Free Software Foundation, 59 Temple Place - Suite 330,
//  Boston, MA 02111-1307, USA.

package gnu.testlet.java.lang.Math;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

/**
  * Test for a static method Math.atan()
  */
public class atan implements Testlet
{
  /**
   * Function (=static method) checked by this test.
   */
  private static double testedFunction(double input)
    {
      return Math.atan(input);
    }

  /**
   * These values are used as arguments to compute atan using Math.
   */
  private static double[] inputValues =
    {
      Double.NaN,
      Double.POSITIVE_INFINITY,
      Double.NEGATIVE_INFINITY,
      Double.MAX_VALUE,
      Double.MIN_VALUE,
      0.0,
      0.2,
      0.4,
      0.5,
      0.6,
      0.8,
      1.0,
      2.0,
      1e10,
      1e-10,
      -0.0,
      -0.2,
      -0.4,
      -0.5,
      -0.6,
      -0.8,
      -1.0,
      -2.0,
      -1e10,
      -1e-10,
    };

  /**
   * These values are the expected results, obtained from the RI.
   */
  private static double[] outputValues =
    {
      // output value      input value
      Double.NaN,          // NaN
      Math.PI/2.0,   // Infinity
      -Math.PI/2.0,  // -Infinity
      1.5707963267948966,  // 1.7976931348623157E308
      4.9E-324,            // 4.9E-324
      0.0,                 // 0.0
      0.19739555984988078, // 0.2
      0.3805063771123649,  // 0.4
      0.4636476090008061,  // 0.5
      0.5404195002705842,  // 0.6
      0.6747409422235527,  // 0.8
      0.7853981633974483,  // 1.0
      1.1071487177940904,  // 2.0
      1.5707963266948965,  // 1.0E10
      1.0E-10,             // 1.0E-10
      -0.0,                // -0.0
      -0.19739555984988078,// -0.2
      -0.3805063771123649, // -0.4
      -0.4636476090008061, // -0.5
      -0.5404195002705842, // -0.6
      -0.6747409422235527, // -0.8
      -0.7853981633974483, // -1.0
      -1.1071487177940904, // -2.0
      -1.5707963266948965, // -1.0E10
      -1.0E-10,            // -1.0E-10
    };

  /**
    * These values represent various NaN
    */
  private static long[] NaNValues =
    {
      0x7fff800000000000L,
      0xffff800000000000L,
      0x7fff812345abcdefL,
      0xffff812345abcdefL,

      0x7fff000000000001L,
      0xffff000000000001L,
      0x7fff7654321fedcbL,
      0xffff7654321fedcbL
    };

  /**
   * Test not NaN values.
   */
  private void testInputValues(TestHarness harness)
  {
    double res;

    for (int i = 0; i < inputValues.length; ++i)
      {
	res = testedFunction(inputValues[i]);

	// exact equality
	harness.check(Double.doubleToLongBits(res), Double.doubleToLongBits(outputValues[i]));
      }
  }

  /**
   * Test if input NaN is returned unchanged.
   */
  private void testNaN(TestHarness harness)
  {
    long   bitsNaN;
    double valNaN;

    for (int i = 0; i < NaNValues.length; ++i)
      {
	bitsNaN = NaNValues[i];
	valNaN  = Double.longBitsToDouble(bitsNaN);

	// exact equality
	harness.check(Double.doubleToRawLongBits(testedFunction(valNaN)),
		      bitsNaN);
      }
  }

  /**
    * Entry point to a test.
    */
  public void test(TestHarness harness)
  {
    testInputValues(harness);
    testNaN(harness);
  }

  /**
   * Run this on the RI to obtain the expected output values.
   */
  public static void main(String[] argv)
  {
    for (int i = 0; i < inputValues.length; ++i)
      {
    double input = inputValues[i];
    double output = testedFunction(inputValues[i]);
	System.out.println("      " + Double.toString(output) + ", // " + input);
      }
  }
}
