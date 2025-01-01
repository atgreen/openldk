// Tags: JDK1.5

//  This file is part of Mauve.
//  update for strictfp modifier Pavel Tisnovsky <ptisnovs@redhat.com>

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

package gnu.testlet.java.lang.Math.strictfp_modifier;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

/**
  * Test for a static method Math.cbrt()
  * by using strictfp modifier.
  */
public strictfp class cbrt implements Testlet
{
  /**
   * These values are used as arguments to cbrt.
   */
  private static double[] inputValues =
    {
      0.0,
      Double.NaN,
      Double.POSITIVE_INFINITY,
      Double.NEGATIVE_INFINITY,
      123456789e-9,
      -123456789e-6,
      123456789e+2,
      -123456789e+4,
      987654321e-7,
      -987654321e-4,
      987654321e+3,
      -987654321e+5,
      1234509876e-320,  // subnormal number
      9756272385e-325,  // subnormal number
      Math.PI,
      Math.E
    };

  /**
   * These values are the expected results, obtained from the RI.
   */
  private static double[] outputValues =
    {
      0.0,
      Double.NaN,
      Double.POSITIVE_INFINITY,
      Double.NEGATIVE_INFINITY,
      0.49793385921817446,
      -4.979338592181745,
      2311.204240824796,
      -10727.659796410873,
      4.622408495690158,
      -46.224084956901585,
      9958.677214612972,
      -46224.08495690158,
      2.3111680380625372e-104,
      9.918088333941088e-106,
      1.4645918875615231,
      1.3956124250860895
    };

  /**
   * Test if input NaN is returned unchanged.
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
	res = Math.cbrt(inputValues[i]);

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
	harness.check(Double.doubleToRawLongBits(Math.cbrt(valNaN)),
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
    double res;

    for (int i = 0; i < inputValues.length; ++i)
      {
	res = Math.cbrt(inputValues[i]);

	System.out.println(Double.toString(res));
      }
  }
}
