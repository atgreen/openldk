// Tags: JDK1.0

/* Copyright (C) 1999 Cygnus Solutions
   Copyright (C) 2012 Pavel Tisnovsky <ptisnovs@redhat.com>

   This file is part of Mauve.

   Mauve is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Mauve is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Mauve; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.lang.Math;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class cos implements Testlet
{
  /**
   * Function (=static method) checked by this test.
   */
  private static double testedFunction(double input)
    {
      return Math.cos(input);
    }

  /**
   * These values are used as arguments to compute cos using Math.
   */
  private static double[] inputValues =
    {
      Double.NaN,
      Double.POSITIVE_INFINITY,
      Double.NEGATIVE_INFINITY,
      Double.MAX_VALUE,
      Double.MIN_VALUE,
      0.0,
      Math.PI/4,
      Math.PI/2,
      Math.PI,
      2*Math.PI,
      4*Math.PI,
      100*Math.PI,
      1e10,
      1e-10,
      -0.0,
      -Math.PI/4,
      -Math.PI/2,
      -Math.PI,
      -2*Math.PI,
      -4*Math.PI,
      -100*Math.PI,
      -1e10,
      -1e-10,
    };

  /**
   * These values are the expected results, obtained from the RI.
   */
  private static double[] outputValues =
    {
      // output value        input value
      Double.NaN,            // NaN
      Double.NaN,            // Infinity
      Double.NaN,            // -Infinity
      -0.9999876894265599,   // Double.MAX_VALUE
      1.0,                   // Double.MIN_VALUE
      1.0,                   // 0.0
      0.7071067811865476,    // Math.PI/4
      6.123233995736766E-17, // Math.PI/2
      -1.0,                  // Math.PI
      1.0,                   // 2*Math.PI
      1.0,                   // 4*Math.PI
      1.0,                   // 100*Math.PI
      0.873119622676856,     // 1.0E10
      1.0,                   // 1.0E-10
      1.0,                   // -0.0
      0.7071067811865476,    // -Math.PI/4
      6.123233995736766E-17, // -Math.PI/2
      -1.0,                  // -Math.PI
      1.0,                   // -2*Math.PI
      1.0,                   // -4*Math.PI
      1.0,                   // -100*Math.PI
      0.873119622676856,     // -1.0E10
      1.0,                   // -1.0E-10
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

  public void test (TestHarness harness)
    {
      harness.check (new Double (Math.cos (0)).toString (), "1.0");
      harness.check (new Double (Math.cos (Math.PI)).toString (), "-1.0");
      harness.check (Math.abs (Math.cos (Math.PI/2))
		     <= 1.1102230246251565E-16); 
      // It's unreasonable to expect the result of this to be eactly
      // zero, but 2^-53, the value of the constant used here, is 1ulp
      // in the range of cos.
      testInputValues(harness);
      testNaN(harness);
    }

}
