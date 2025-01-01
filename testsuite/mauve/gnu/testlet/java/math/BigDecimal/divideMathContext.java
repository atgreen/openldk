// Test of the method BigDecimal.divideMathContext()

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
//
// Tags: JDK1.5

package gnu.testlet.java.math.BigDecimal;

import java.math.BigDecimal;
import java.math.MathContext;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

/**
 * Check for method BigDecimal.divideMathContext()
 */
public class divideMathContext implements Testlet
{
  public void test (TestHarness harness)
  {
    BigDecimal a = new BigDecimal("0");
    BigDecimal b = a;
    BigDecimal result;

    harness.checkPoint("basic tests");
    a = new BigDecimal("10");
    b = new BigDecimal("2");
    result = new BigDecimal("5");
    harness.check(a.divide(b, new MathContext(0)), result);
    a = a.negate();
    b = b.negate();
    result = new BigDecimal("5");
    harness.check(a.divide(b, new MathContext(0)), result);
    b = b.negate();
    result = new BigDecimal("-5");
    harness.check(a.divide(b, new MathContext(0)), result);

    harness.checkPoint("rounding to one");
    a = new BigDecimal("1000000000000000000");
    b = new BigDecimal("1000000000000000000");
    result = new BigDecimal("1");
    harness.check(a.divide(b, new MathContext(0)), result);
    a = a.negate();
    b = b.negate();
    result = new BigDecimal("1");
    harness.check(a.divide(b, new MathContext(0)), result);
    b = b.negate();
    result = new BigDecimal("-1");
    harness.check(a.divide(b, new MathContext(0)), result);

    harness.checkPoint("rounding to zero");
    a = new BigDecimal("10");
    b = new BigDecimal("20");
    result = new BigDecimal("0.5");
    harness.check(a.divide(b, new MathContext(0)), result);
    a = a.negate();
    b = b.negate();
    result = new BigDecimal("0.5");
    harness.check(a.divide(b, new MathContext(0)), result);
    b = b.negate();
    result = new BigDecimal("-0.5");
    harness.check(a.divide(b, new MathContext(0)), result);

    harness.checkPoint("rounding with division");
    a = new BigDecimal("20.9");
    b = new BigDecimal("10.1");
    result = new BigDecimal("2");
    try {
      a.divide(b, new MathContext(0));
      harness.fail("ArithmeticException not thrown as expected");
    }
    catch (ArithmeticException e) {
      // ok
    }

    harness.checkPoint("rounding with division ");
    a = new BigDecimal("20.1");
    b = new BigDecimal("10.9");
    result = new BigDecimal("2");
    try {
      a.divide(b, new MathContext(0));
      harness.fail("ArithmeticException not thrown as expected");
    }
    catch (ArithmeticException e) {
      // ok
    }

    harness.checkPoint("rounding with division ");
    a = new BigDecimal("20.000000000000000000001");
    b = new BigDecimal("10.000000000000000000001");
    result = new BigDecimal("2");
    try {
      a.divide(b, new MathContext(0));
      harness.fail("ArithmeticException not thrown as expected");
    }
    catch (ArithmeticException e) {
      // ok
    }
  }
}

