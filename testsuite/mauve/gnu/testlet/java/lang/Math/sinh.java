// Tags: JDK1.5

//  Copyright (C) 2006 Carsten Neumann <cn-develop@gmx.net>

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

public class sinh implements Testlet
{
  /**
   * These values are used as arguments to sinh.
   * The values are somewhat arbitrary, but ensure that all code paths
   * are tested.
   */
  private static double[] inputValues =
    {
      0.0,
      Double.NaN,
      Double.POSITIVE_INFINITY,
      Double.NEGATIVE_INFINITY,
      0.123456789,
      -0.123456789,
      0.234242656456,
      -0.234242656456,
      1.23456789,
      -1.23456789,
      9.87654321,
      -9.87654321,
      21.9999,
      -21.9999,
      567.891234,
      -567.891234,
      708.742342,
      -708.742342,
      709.7827128,
      -709.7827128,
      710.475860073943,
      710.4755859375,
      723.6787676346,
      -723.6787676346,
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
      0.1237706408130359,
      -0.1237706408130359,
      0.23639067538469039,
      -0.23639067538469039,
      1.5729663108942873,
      -1.5729663108942873,
      9734.154152818386,
      -9734.154152818386,
      1.792277186385473E9,
      -1.792277186385473E9,
      2.1428869091881118E246,
      -2.1428869091881118E246,
      3.1758371607555525E307,
      -3.1758371607555525E307,
      8.988464834932886E307,
      -8.988464834932886E307,
      1.7976931348605396E308,
      1.7972003892018829E308,
      Double.POSITIVE_INFINITY,
      Double.NEGATIVE_INFINITY,
    };

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

  private void testInputValues(TestHarness harness)
  {
    double res;

    for (int i = 0; i < inputValues.length; ++i)
      {
	res = Math.sinh(inputValues[i]);

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
	harness.check(Double.doubleToRawLongBits(Math.sinh(valNaN)),
		      bitsNaN);
      }
  }

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
	res = Math.sinh(inputValues[i]);

	System.out.println(Double.toString(res));
      }
  }
}
