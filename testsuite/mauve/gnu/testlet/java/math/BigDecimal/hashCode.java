// Test of BigDecimal method hashCode().

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

// Tags: JDK1.4

package gnu.testlet.java.math.BigDecimal;

import java.math.BigDecimal;
import java.math.BigInteger;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

/**
 * Test the method BigDecimal.hashCode();
 * Partially based on test compareTo(BigDecimal)
 */
public class hashCode implements Testlet
{
  /**
   * Entry point to this test.
   */
  public void test(TestHarness harness)
  {
    positiveTest(harness);
    negativeTest(harness);
  }

  /**
   * Positive test for method BigDecimal.hashCode()
   */
  public void positiveTest(TestHarness harness)
  {
    BigDecimal a, b;

    a = new BigDecimal("0");
    b = new BigDecimal("0");
    harness.check(a.hashCode(), b.hashCode());

    a = BigDecimal.ZERO;
    b = new BigDecimal("0");
    harness.check(a.hashCode(), b.hashCode());

    a = BigDecimal.ZERO;
    b = new BigDecimal(0);
    harness.check(a.hashCode(), b.hashCode());

    a = new BigDecimal("1");
    b = new BigDecimal("1");
    harness.check(a.hashCode(), b.hashCode());

    a = BigDecimal.ONE;
    b = new BigDecimal("1");
    harness.check(a.hashCode(), b.hashCode());

    a = BigDecimal.ONE;
    b = new BigDecimal(1);
    harness.check(a.hashCode(), b.hashCode());

    a = new BigDecimal("0.1");
    b = new BigDecimal("0.1");
    harness.check(a.hashCode(), b.hashCode());

    a = new BigDecimal(10);
    b = BigDecimal.TEN;
    harness.check(a.hashCode(), b.hashCode());
  }

  /**
   * Negative test for method BigDecimal.hashCode()
   */
  public void negativeTest(TestHarness harness)
  {
    BigDecimal a, b;

    a = new BigDecimal("0.1");
    b = new BigDecimal("0.10");
    harness.check(a.hashCode() != b.hashCode());

    a = new BigDecimal("10.0");
    b = BigDecimal.TEN;
    harness.check(a.hashCode() != b.hashCode());

    a = new BigDecimal("10.0");
    b = new BigDecimal("10.00");
    harness.check(a.hashCode() != b.hashCode());
  }
}

