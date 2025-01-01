// Tags: JDK1.5

//  Copyright (C) 2006 Carsten Neumann <cn-develop@gmx.net>
//  update for strictfp class Pavel Tisnovsky <ptisnovs@redhat.com>

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

package gnu.testlet.java.lang.StrictMath;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public strictfp class cosh_strictfp implements Testlet
{
  /**
   * These values are used as arguments to cosh.
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
      1.0,
      Double.NaN,
      Double.POSITIVE_INFINITY,
      Double.POSITIVE_INFINITY,
      1.0076304736991977,
      1.0076304736991977,
      1.0275604855232756,
      1.0275604855232756,
      1.8639267730274125,
      1.8639267730274125,
      9734.154204183918,
      9734.154204183918,
      1.792277186385473e9,
      1.792277186385473e9,
      2.1428869091881118e246,
      2.1428869091881118e246,
      3.1758371607555525e307,
      3.1758371607555525e307,
      8.988464834932886e307,
      8.988464834932886e307,
      1.7976931348605396e308,
      1.7972003892018829e308,
      Double.POSITIVE_INFINITY,
      Double.POSITIVE_INFINITY
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
	res = StrictMath.cosh(inputValues[i]);

	// exact equality seems appropriate for StrictMath
	harness.check(res, outputValues[i]);
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

	harness.check(Double.doubleToRawLongBits(StrictMath.cosh(valNaN)),
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
	res = StrictMath.cosh(inputValues[i]);

	System.out.println(Double.toString(res));
      }
  }
}

