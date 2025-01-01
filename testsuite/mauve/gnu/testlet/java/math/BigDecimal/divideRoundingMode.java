// Test of the method BigDecimal.divide(BigDecimal,RoundingMode)

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
import java.math.RoundingMode;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

/**
 * Check for method BigDecimal.divide(BigDecimal,RoundingMode)
 */
public class divideRoundingMode implements Testlet
{
  public void test (TestHarness harness)
  {
    BigDecimal a;
    BigDecimal b;
    BigDecimal result;

    harness.checkPoint("basic tests");
    a = new BigDecimal("10");
    b = new BigDecimal("3");

    result = new BigDecimal("4");
    harness.check(a.divide(b, RoundingMode.UP), result);

    result = new BigDecimal("3");
    harness.check(a.divide(b, RoundingMode.DOWN), result);

    result = new BigDecimal("4");
    harness.check(a.divide(b, RoundingMode.CEILING), result);

    result = new BigDecimal("3");
    harness.check(a.divide(b, RoundingMode.FLOOR), result);

    result = new BigDecimal("3");
    harness.check(a.divide(b, RoundingMode.HALF_UP), result);

    result = new BigDecimal("3");
    harness.check(a.divide(b, RoundingMode.HALF_DOWN), result);

    result = new BigDecimal("3");
    harness.check(a.divide(b, RoundingMode.HALF_EVEN), result);



    harness.checkPoint("negative result");
    a = new BigDecimal("10");
    b = new BigDecimal("-3");

    result = new BigDecimal("-4");
    harness.check(a.divide(b, RoundingMode.UP), result);

    result = new BigDecimal("-3");
    harness.check(a.divide(b, RoundingMode.DOWN), result);

    result = new BigDecimal("-3");
    harness.check(a.divide(b, RoundingMode.CEILING), result);

    result = new BigDecimal("-4");
    harness.check(a.divide(b, RoundingMode.FLOOR), result);

    result = new BigDecimal("-3");
    harness.check(a.divide(b, RoundingMode.HALF_UP), result);

    result = new BigDecimal("-3");
    harness.check(a.divide(b, RoundingMode.HALF_DOWN), result);

    result = new BigDecimal("-3");
    harness.check(a.divide(b, RoundingMode.HALF_EVEN), result);



    harness.checkPoint("negative result, second test case");
    a = new BigDecimal("-10");
    b = new BigDecimal("3");

    result = new BigDecimal("-4");
    harness.check(a.divide(b, RoundingMode.UP), result);

    result = new BigDecimal("-3");
    harness.check(a.divide(b, RoundingMode.DOWN), result);

    result = new BigDecimal("-3");
    harness.check(a.divide(b, RoundingMode.CEILING), result);

    result = new BigDecimal("-4");
    harness.check(a.divide(b, RoundingMode.FLOOR), result);

    result = new BigDecimal("-3");
    harness.check(a.divide(b, RoundingMode.HALF_UP), result);

    result = new BigDecimal("-3");
    harness.check(a.divide(b, RoundingMode.HALF_DOWN), result);

    result = new BigDecimal("-3");
    harness.check(a.divide(b, RoundingMode.HALF_EVEN), result);
  }
}

