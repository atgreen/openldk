// Tags: JDK1.1

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Mark Wielaard (mark@klomp.org)
 
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
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

package gnu.testlet.java.math.BigDecimal;

import java.math.BigDecimal;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class divide implements Testlet
{
  public void test (TestHarness harness)
  {
    BigDecimal a = new BigDecimal("0");
    BigDecimal b = a;

    /*
    harness.checkPoint("zero / zero");
    System.out.println("a: " + a + " b: " + b);
    System.out.println("a.scale(): " + a.scale());
    harness.check(a.divide(b, BigDecimal.ROUND_UP), a);

    a = new BigDecimal("0.0");
    System.out.println("a: " + a + " b: " + b);
    System.out.println("a.scale(): " + a.scale());
    harness.check(a.divide(b, BigDecimal.ROUND_DOWN), a);

    b = new BigDecimal("0.00");
    System.out.println("a: " + a + " b: " + b);
    System.out.println("b.scale(): " + b.scale());
    harness.check(a.divide(b, BigDecimal.ROUND_CEILING), a);
    */

    harness.checkPoint("basic tests");
    a = new BigDecimal("10");
    b = new BigDecimal("2");
    BigDecimal result = new BigDecimal("5");
    harness.check(a.divide(b), result);
    a = a.negate();
    b = b.negate();
    result = new BigDecimal("5");
    harness.check(a.divide(b), result);
    b = b.negate();
    result = new BigDecimal("-5");
    harness.check(a.divide(b), result);

    harness.checkPoint("unrounded zero");
    a = new BigDecimal("9");
    b = new BigDecimal("-100");
    result = new BigDecimal("0");
    harness.check(a.divide(b, BigDecimal.ROUND_CEILING), result);
    result = new BigDecimal("-1");
    harness.check(a.divide(b, BigDecimal.ROUND_FLOOR), result);
    a = a.negate();
    b = b.negate();
    result = new BigDecimal("0");
    harness.check(a.divide(b, BigDecimal.ROUND_CEILING), result);
    result = new BigDecimal("-1");
    harness.check(a.divide(b, BigDecimal.ROUND_FLOOR), result);
    
    a = new BigDecimal("66.70");
    b = new BigDecimal("2");
    result = new BigDecimal("33.35");
    harness.checkPoint("66.70 / 2");
    harness.check(a.divide(b, BigDecimal.ROUND_UP), result);
    harness.check(a.divide(b, BigDecimal.ROUND_DOWN), result);
    harness.check(a.divide(b, BigDecimal.ROUND_CEILING), result);
    harness.check(a.divide(b, BigDecimal.ROUND_FLOOR), result);
    harness.check(a.divide(b, BigDecimal.ROUND_HALF_UP), result);
    harness.check(a.divide(b, BigDecimal.ROUND_HALF_DOWN), result);
    harness.check(a.divide(b, BigDecimal.ROUND_HALF_EVEN), result);
    harness.check(a.divide(b, BigDecimal.ROUND_UNNECESSARY), result);

    result = new BigDecimal("33");
    BigDecimal result_up = new BigDecimal("34");
    harness.checkPoint("66.70 / 2, scale 0");
    harness.check(a.divide(b, 0, BigDecimal.ROUND_UP), result_up);
    harness.check(a.divide(b, 0, BigDecimal.ROUND_DOWN), result);
    harness.check(a.divide(b, 0, BigDecimal.ROUND_CEILING), result_up);
    harness.check(a.divide(b, 0, BigDecimal.ROUND_FLOOR), result);
    harness.check(a.divide(b, 0, BigDecimal.ROUND_HALF_UP), result);
    harness.check(a.divide(b, 0, BigDecimal.ROUND_HALF_DOWN), result);
    harness.check(a.divide(b, 0, BigDecimal.ROUND_HALF_EVEN), result);
    boolean exception = false;
    try
      {
	a.divide(b, 0, BigDecimal.ROUND_UNNECESSARY);
      }
    catch (ArithmeticException ae)
      {
	exception = true;
      }
    harness.check(exception);

    result = new BigDecimal("33.3");
    result_up = new BigDecimal("33.4");
    harness.checkPoint("66.70 / 2, scale 1");
    harness.check(a.divide(b, 1, BigDecimal.ROUND_UP), result_up);
    harness.check(a.divide(b, 1, BigDecimal.ROUND_DOWN), result);
    harness.check(a.divide(b, 1, BigDecimal.ROUND_CEILING), result_up);
    harness.check(a.divide(b, 1, BigDecimal.ROUND_FLOOR), result);
    harness.check(a.divide(b, 1, BigDecimal.ROUND_HALF_UP), result_up);
    harness.check(a.divide(b, 1, BigDecimal.ROUND_HALF_DOWN), result);
    harness.check(a.divide(b, 1, BigDecimal.ROUND_HALF_EVEN), result_up);
    exception = false;
    try
      {
	a.divide(b, 1, BigDecimal.ROUND_UNNECESSARY);
      }
    catch (ArithmeticException ae)
      {
	exception = true;
      }
    harness.check(exception);

    result = new BigDecimal("33.35");
    harness.checkPoint("66.70 / 2, scale 2");
    harness.check(a.divide(b, 2, BigDecimal.ROUND_UP), result);
    harness.check(a.divide(b, 2, BigDecimal.ROUND_DOWN), result);
    harness.check(a.divide(b, 2, BigDecimal.ROUND_CEILING), result);
    harness.check(a.divide(b, 2, BigDecimal.ROUND_FLOOR), result);
    harness.check(a.divide(b, 2, BigDecimal.ROUND_HALF_UP), result);
    harness.check(a.divide(b, 2, BigDecimal.ROUND_HALF_DOWN), result);
    harness.check(a.divide(b, 2, BigDecimal.ROUND_HALF_EVEN), result);
    harness.check(a.divide(b, 2, BigDecimal.ROUND_UNNECESSARY), result);

    result = new BigDecimal("33.350");
    harness.checkPoint("66.70 / 2, scale 3");
    harness.check(a.divide(b, 3, BigDecimal.ROUND_UP), result);
    harness.check(a.divide(b, 3, BigDecimal.ROUND_DOWN), result);
    harness.check(a.divide(b, 3, BigDecimal.ROUND_CEILING), result);
    harness.check(a.divide(b, 3, BigDecimal.ROUND_FLOOR), result);
    harness.check(a.divide(b, 3, BigDecimal.ROUND_HALF_UP), result);
    harness.check(a.divide(b, 3, BigDecimal.ROUND_HALF_DOWN), result);
    harness.check(a.divide(b, 3, BigDecimal.ROUND_HALF_EVEN), result);
    harness.check(a.divide(b, 3, BigDecimal.ROUND_UNNECESSARY), result);

    a = new BigDecimal("-66.70");
    result = new BigDecimal("-33.35");
    harness.checkPoint("-66.70 / 2");
    harness.check(a.divide(b, BigDecimal.ROUND_UP), result);
    harness.check(a.divide(b, BigDecimal.ROUND_DOWN), result);
    harness.check(a.divide(b, BigDecimal.ROUND_CEILING), result);
    harness.check(a.divide(b, BigDecimal.ROUND_FLOOR), result);
    harness.check(a.divide(b, BigDecimal.ROUND_HALF_UP), result);
    harness.check(a.divide(b, BigDecimal.ROUND_HALF_DOWN), result);
    harness.check(a.divide(b, BigDecimal.ROUND_HALF_EVEN), result);
    harness.check(a.divide(b, BigDecimal.ROUND_UNNECESSARY), result);

    result = new BigDecimal("-33");
    result_up = new BigDecimal("-34");
    harness.checkPoint("-66.70 / 2, scale 0");
    harness.check(a.divide(b, 0, BigDecimal.ROUND_UP), result_up);
    harness.check(a.divide(b, 0, BigDecimal.ROUND_DOWN), result);
    harness.check(a.divide(b, 0, BigDecimal.ROUND_CEILING), result);
    harness.check(a.divide(b, 0, BigDecimal.ROUND_FLOOR), result_up);
    harness.check(a.divide(b, 0, BigDecimal.ROUND_HALF_UP), result);
    harness.check(a.divide(b, 0, BigDecimal.ROUND_HALF_DOWN), result);
    harness.check(a.divide(b, 0, BigDecimal.ROUND_HALF_EVEN), result);
    exception = false;
    try
      {
	a.divide(b, 0, BigDecimal.ROUND_UNNECESSARY);
      }
    catch (ArithmeticException ae)
      {
	exception = true;
      }
    harness.check(exception);

    result = new BigDecimal("-33.3");
    result_up = new BigDecimal("-33.4");
    harness.checkPoint("-66.70 / 2, scale 1");
    harness.check(a.divide(b, 1, BigDecimal.ROUND_UP), result_up);
    harness.check(a.divide(b, 1, BigDecimal.ROUND_DOWN), result);
    harness.check(a.divide(b, 1, BigDecimal.ROUND_CEILING), result);
    harness.check(a.divide(b, 1, BigDecimal.ROUND_FLOOR), result_up);
    harness.check(a.divide(b, 1, BigDecimal.ROUND_HALF_UP), result_up);
    harness.check(a.divide(b, 1, BigDecimal.ROUND_HALF_DOWN), result);
    harness.check(a.divide(b, 1, BigDecimal.ROUND_HALF_EVEN), result_up);
    exception = false;
    try
      {
	a.divide(b, 1, BigDecimal.ROUND_UNNECESSARY);
      }
    catch (ArithmeticException ae)
      {
	exception = true;
      }
    harness.check(exception);

    result = new BigDecimal("-33.35");
    harness.checkPoint("-66.70 / 2, scale 2");
    harness.check(a.divide(b, 2, BigDecimal.ROUND_UP), result);
    harness.check(a.divide(b, 2, BigDecimal.ROUND_DOWN), result);
    harness.check(a.divide(b, 2, BigDecimal.ROUND_CEILING), result);
    harness.check(a.divide(b, 2, BigDecimal.ROUND_FLOOR), result);
    harness.check(a.divide(b, 2, BigDecimal.ROUND_HALF_UP), result);
    harness.check(a.divide(b, 2, BigDecimal.ROUND_HALF_DOWN), result);
    harness.check(a.divide(b, 2, BigDecimal.ROUND_HALF_EVEN), result);
    harness.check(a.divide(b, 2, BigDecimal.ROUND_UNNECESSARY), result);

    result = new BigDecimal("-33.350");
    harness.checkPoint("-66.70 / 2, scale 3");
    harness.check(a.divide(b, 3, BigDecimal.ROUND_UP), result);
    harness.check(a.divide(b, 3, BigDecimal.ROUND_DOWN), result);
    harness.check(a.divide(b, 3, BigDecimal.ROUND_CEILING), result);
    harness.check(a.divide(b, 3, BigDecimal.ROUND_FLOOR), result);
    harness.check(a.divide(b, 3, BigDecimal.ROUND_HALF_UP), result);
    harness.check(a.divide(b, 3, BigDecimal.ROUND_HALF_DOWN), result);
    harness.check(a.divide(b, 3, BigDecimal.ROUND_HALF_EVEN), result);
    harness.check(a.divide(b, 3, BigDecimal.ROUND_UNNECESSARY), result);

    a = new BigDecimal("66.70");
    b = new BigDecimal("-2");
    result = new BigDecimal("-33.35");
    harness.checkPoint("66.70 / -2");
    harness.check(a.divide(b, BigDecimal.ROUND_UP), result);
    harness.check(a.divide(b, BigDecimal.ROUND_DOWN), result);
    harness.check(a.divide(b, BigDecimal.ROUND_CEILING), result);
    harness.check(a.divide(b, BigDecimal.ROUND_FLOOR), result);
    harness.check(a.divide(b, BigDecimal.ROUND_HALF_UP), result);
    harness.check(a.divide(b, BigDecimal.ROUND_HALF_DOWN), result);
    harness.check(a.divide(b, BigDecimal.ROUND_HALF_EVEN), result);
    harness.check(a.divide(b, BigDecimal.ROUND_UNNECESSARY), result);

    result = new BigDecimal("-33");
    result_up = new BigDecimal("-34");
    harness.checkPoint("66.70 / -2, scale 0");
    harness.check(a.divide(b, 0, BigDecimal.ROUND_UP), result_up);
    harness.check(a.divide(b, 0, BigDecimal.ROUND_DOWN), result);
    harness.check(a.divide(b, 0, BigDecimal.ROUND_CEILING), result);
    harness.check(a.divide(b, 0, BigDecimal.ROUND_FLOOR), result_up);
    harness.check(a.divide(b, 0, BigDecimal.ROUND_HALF_UP), result);
    harness.check(a.divide(b, 0, BigDecimal.ROUND_HALF_DOWN), result);
    harness.check(a.divide(b, 0, BigDecimal.ROUND_HALF_EVEN), result);
    exception = false;
    try
      {
	a.divide(b, 0, BigDecimal.ROUND_UNNECESSARY);
      }
    catch (ArithmeticException ae)
      {
	exception = true;
      }
    harness.check(exception);

    result = new BigDecimal("-33.3");
    result_up = new BigDecimal("-33.4");
    harness.checkPoint("66.70 / -2, scale 1");
    harness.check(a.divide(b, 1, BigDecimal.ROUND_UP), result_up);
    harness.check(a.divide(b, 1, BigDecimal.ROUND_DOWN), result);
    harness.check(a.divide(b, 1, BigDecimal.ROUND_CEILING), result);
    harness.check(a.divide(b, 1, BigDecimal.ROUND_FLOOR), result_up);
    harness.check(a.divide(b, 1, BigDecimal.ROUND_HALF_UP), result_up);
    harness.check(a.divide(b, 1, BigDecimal.ROUND_HALF_DOWN), result);
    harness.check(a.divide(b, 1, BigDecimal.ROUND_HALF_EVEN), result_up);
    exception = false;
    try
      {
	a.divide(b, 1, BigDecimal.ROUND_UNNECESSARY);
      }
    catch (ArithmeticException ae)
      {
	exception = true;
      }
    harness.check(exception);

    result = new BigDecimal("-33.35");
    harness.checkPoint("66.70 / -2, scale 2");
    harness.check(a.divide(b, 2, BigDecimal.ROUND_UP), result);
    harness.check(a.divide(b, 2, BigDecimal.ROUND_DOWN), result);
    harness.check(a.divide(b, 2, BigDecimal.ROUND_CEILING), result);
    harness.check(a.divide(b, 2, BigDecimal.ROUND_FLOOR), result);
    harness.check(a.divide(b, 2, BigDecimal.ROUND_HALF_UP), result);
    harness.check(a.divide(b, 2, BigDecimal.ROUND_HALF_DOWN), result);
    harness.check(a.divide(b, 2, BigDecimal.ROUND_HALF_EVEN), result);
    harness.check(a.divide(b, 2, BigDecimal.ROUND_UNNECESSARY), result);

    result = new BigDecimal("-33.350");
    harness.checkPoint("66.70 / -2, scale 3");
    harness.check(a.divide(b, 3, BigDecimal.ROUND_UP), result);
    harness.check(a.divide(b, 3, BigDecimal.ROUND_DOWN), result);
    harness.check(a.divide(b, 3, BigDecimal.ROUND_CEILING), result);
    harness.check(a.divide(b, 3, BigDecimal.ROUND_FLOOR), result);
    harness.check(a.divide(b, 3, BigDecimal.ROUND_HALF_UP), result);
    harness.check(a.divide(b, 3, BigDecimal.ROUND_HALF_DOWN), result);
    harness.check(a.divide(b, 3, BigDecimal.ROUND_HALF_EVEN), result);
    harness.check(a.divide(b, 3, BigDecimal.ROUND_UNNECESSARY), result);

    a = new BigDecimal("-66.70");
    result = new BigDecimal("33.35");
    harness.checkPoint("-66.70 / -2");
    harness.check(a.divide(b, BigDecimal.ROUND_UP), result);
    harness.check(a.divide(b, BigDecimal.ROUND_DOWN), result);
    harness.check(a.divide(b, BigDecimal.ROUND_CEILING), result);
    harness.check(a.divide(b, BigDecimal.ROUND_FLOOR), result);
    harness.check(a.divide(b, BigDecimal.ROUND_HALF_UP), result);
    harness.check(a.divide(b, BigDecimal.ROUND_HALF_DOWN), result);
    harness.check(a.divide(b, BigDecimal.ROUND_HALF_EVEN), result);
    harness.check(a.divide(b, BigDecimal.ROUND_UNNECESSARY), result);

    result = new BigDecimal("33");
    result_up = new BigDecimal("34");
    harness.checkPoint("-66.70 / -2, scale 0");
    harness.check(a.divide(b, 0, BigDecimal.ROUND_UP), result_up);
    harness.check(a.divide(b, 0, BigDecimal.ROUND_DOWN), result);
    harness.check(a.divide(b, 0, BigDecimal.ROUND_CEILING), result_up);
    harness.check(a.divide(b, 0, BigDecimal.ROUND_FLOOR), result);
    harness.check(a.divide(b, 0, BigDecimal.ROUND_HALF_UP), result);
    harness.check(a.divide(b, 0, BigDecimal.ROUND_HALF_DOWN), result);
    harness.check(a.divide(b, 0, BigDecimal.ROUND_HALF_EVEN), result);
    exception = false;
    try
      {
	a.divide(b, 0, BigDecimal.ROUND_UNNECESSARY);
      }
    catch (ArithmeticException ae)
      {
	exception = true;
      }
    harness.check(exception);

    result = new BigDecimal("33.3");
    result_up = new BigDecimal("33.4");
    harness.checkPoint("-66.70 / -2, scale 1");
    harness.check(a.divide(b, 1, BigDecimal.ROUND_UP), result_up);
    harness.check(a.divide(b, 1, BigDecimal.ROUND_DOWN), result);
    harness.check(a.divide(b, 1, BigDecimal.ROUND_CEILING), result_up);
    harness.check(a.divide(b, 1, BigDecimal.ROUND_FLOOR), result);
    harness.check(a.divide(b, 1, BigDecimal.ROUND_HALF_UP), result_up);
    harness.check(a.divide(b, 1, BigDecimal.ROUND_HALF_DOWN), result);
    harness.check(a.divide(b, 1, BigDecimal.ROUND_HALF_EVEN), result_up);
    exception = false;
    try
      {
	a.divide(b, 1, BigDecimal.ROUND_UNNECESSARY);
      }
    catch (ArithmeticException ae)
      {
	exception = true;
      }
    harness.check(exception);

    result = new BigDecimal("33.35");
    harness.checkPoint("-66.70 / -2, scale 2");
    harness.check(a.divide(b, 2, BigDecimal.ROUND_UP), result);
    harness.check(a.divide(b, 2, BigDecimal.ROUND_DOWN), result);
    harness.check(a.divide(b, 2, BigDecimal.ROUND_CEILING), result);
    harness.check(a.divide(b, 2, BigDecimal.ROUND_FLOOR), result);
    harness.check(a.divide(b, 2, BigDecimal.ROUND_HALF_UP), result);
    harness.check(a.divide(b, 2, BigDecimal.ROUND_HALF_DOWN), result);
    harness.check(a.divide(b, 2, BigDecimal.ROUND_HALF_EVEN), result);
    harness.check(a.divide(b, 2, BigDecimal.ROUND_UNNECESSARY), result);

    result = new BigDecimal("33.350");
    harness.checkPoint("-66.70 / -2, scale 3");
    harness.check(a.divide(b, 3, BigDecimal.ROUND_UP), result);
    harness.check(a.divide(b, 3, BigDecimal.ROUND_DOWN), result);
    harness.check(a.divide(b, 3, BigDecimal.ROUND_CEILING), result);
    harness.check(a.divide(b, 3, BigDecimal.ROUND_FLOOR), result);
    harness.check(a.divide(b, 3, BigDecimal.ROUND_HALF_UP), result);
    harness.check(a.divide(b, 3, BigDecimal.ROUND_HALF_DOWN), result);
    harness.check(a.divide(b, 3, BigDecimal.ROUND_HALF_EVEN), result);
    harness.check(a.divide(b, 3, BigDecimal.ROUND_UNNECESSARY), result);

  }
}
