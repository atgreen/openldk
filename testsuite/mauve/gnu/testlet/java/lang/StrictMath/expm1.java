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

package gnu.testlet.java.lang.StrictMath;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

/**
  * Test for a static method StrictMath.expm1()
  */
public class expm1 implements Testlet
{
  /**
   * Function (=static method) checked by this test.
   */
  private static double testedFunction(double input)
    {
      return StrictMath.expm1(input);
    }

  /**
   * These values are used as arguments to expm1.
   * The values are somewhat arbitrary, but ensure that all code paths
   * are tested.
   */
  private static double[] inputValues =
    {
      0.0,
      Double.NaN,
      Double.POSITIVE_INFINITY,
      Double.NEGATIVE_INFINITY,
      0.0000000000000000277555756156289135105,   // ~ 2^-55
      -0.0000000000000000277555756156289135105,  // ~ -2^55
      0.6 * 0.6931471805599453 + 0.05,           // 0.6 * ln(2) + 0.05
      -0.6 * 0.6931471805599453 - 0.05,          // -0.6 * ln(2) - 0.05
      0.25 * 0.6931471805599453 + 0.03,          // 0.25 * ln(2) + 0.03
      -0.25 * 0.6931471805599453 - 0.03,         // -0.25 * ln(2) - 0.03
      0.44,
      -0.44,
      2.3 * 0.6931471805599453 + 0.05,           // 2.3 * ln(2) + 0.05
      -2.3 * 0.6931471805599453 - 0.05,          // -2.3 * ln(2) - 0.05
      7 * 0.6931471805599453 + 0.03,             // 7 * ln(2) + 0.03
      -9 * 0.6931471805599453 - 0.03,            // -9 * ln(2) - 0.03
      29 * 0.6931471805599453 + 0.03,            // 29 * ln(2) + 0.03
      -27 * 0.6931471805599453 - 0.03,           // -27 * ln(2) - 0.03
      709.782712893384,                          //  EXP_LIMIT_H
      709.782712893384 + 3.423e-5,               //  EXP_LIMIT_H + 3.423e-5
      709.782712893384 - 3.423e-5,               //  EXP_LIMIT_H - 3.423e-5
      -709.782712893384,                         // -EXP_LIMIT_H
      -709.782712893384 - 3.423e-5,              // -EXP_LIMIT_H - 3.423e-5
      -709.782712893384 + 3.423e-5               // -EXP_LIMIT_H + 3.423e-5
    };

  /**
   * These values are the expected results, obtained from the RI.
   */
  private static double[] outputValues =
    {
      0.0,
      Double.NaN,
      Double.POSITIVE_INFINITY,
      -1.0,
      2.7755575615628914E-17,
      -2.7755575615628914E-17,
      0.5934290166706889,
      -0.3724226247056801,
      0.22542386346433524,
      -0.1839558296400811,
      0.5527072185113361,
      -0.35596357891685865,
      4.177066148857307,
      -0.806840405116183,
      130.89818034605017,
      -0.9981045985672881,
      5.532210644181606E8,
      -0.9999999927696174,
      1.7976931348622732E308,
      Double.POSITIVE_INFINITY,
      1.7976316008794578E308,
      -1.0,
      -1.0,
      -1.0
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

