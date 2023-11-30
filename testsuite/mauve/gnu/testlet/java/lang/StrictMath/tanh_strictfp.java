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

public strictfp class tanh_strictfp implements Testlet
{
  /**
   * These values are used as arguments to tanh.
   * The values are somewhat arbitrary, but ensure that all code paths
   * are tested.
   */
  private static double[] inputValues =
    {
      0.0,
      Double.NaN,
      Double.POSITIVE_INFINITY,
      Double.NEGATIVE_INFINITY,
      2.7755575615628e-16,            // ~ 2^-55
      -2.7755575615628e-16,           // ~ -2^-55
      2.5123456789e-16,               // < 2^-55
      -2.5123456789e-16,              // > -2^-55
      0.123456789,
      -0.123456789,
      0.987654321,
      -0.987654321,
      1.0000000000000000123,
      -1.0000000000000000123,
      1.123456789,
      -1.123456789,
      21.999999999999999876,
      -21.999999999999999876,
      22.000000000000000123,
      -22.000000000000000123,
      25.987654321,
      -25.987654321,
    };

    /**
   * These values are the expected results, obtained from the RI.
   */
  private static double[] outputValues =
    {
      0.0,
      Double.NaN,
      1.0,
      -1.0,
      2.7755575615627996E-16,
      -2.7755575615627996E-16,
      2.5123456789E-16,
      -2.5123456789E-16,
      0.12283336405919822,
      -0.12283336405919822,
      0.7563603430619676,
      -0.7563603430619676,
      0.7615941559557649,
      -0.7615941559557649,
      0.8087679479619252,
      -0.8087679479619252,
      1.0,
      -1.0,
      1.0,
      -1.0,
      1.0,
      -1.0,
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
	res = StrictMath.tanh(inputValues[i]);

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

	harness.check(Double.doubleToRawLongBits(StrictMath.tanh(valNaN)),
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
	res = StrictMath.tanh(inputValues[i]);

	System.out.println(Double.toString(res));
      }
  }
}

