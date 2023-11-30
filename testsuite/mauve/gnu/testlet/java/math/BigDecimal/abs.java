// Test of the method BigDecimal.abs()

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
  * Check for method BigDecimal.abs()
  */
public class abs implements Testlet
{
  /**
   * Entry point to this test.
   */
  public void test(TestHarness harness)
  {
    test1(harness);
    test2(harness, new MathContext(0));
    test2(harness, new MathContext(1));
    test2(harness, new MathContext(2));
    test3(harness);
  }

  /**
   * Basic tests
   */
  private void test1(TestHarness harness)
  {
    harness.check(((new BigDecimal("2")).abs().toString()).equals("2"));
    harness.check(((new BigDecimal("-2")).abs().toString()).equals("2"));
    harness.check(((new BigDecimal("+0.000")).abs().toString()).equals("0.000"));
    harness.check(((new BigDecimal("00.000")).abs().toString()).equals("0.000"));
    harness.check(((new BigDecimal("-0.000")).abs().toString()).equals("0.000"));
    harness.check(((new BigDecimal("-2000000")).abs().toString()).equals("2000000"));
    harness.check(((new BigDecimal("0.2")).abs().toString()).equals("0.2"));
    harness.check(((new BigDecimal("-0.2")).abs().toString()).equals("0.2"));
    harness.check(((new BigDecimal("0.01")).abs().toString()).equals("0.01"));
    harness.check(((new BigDecimal("-0.01")).abs().toString()).equals("0.01"));
  }

  /**
   * Tests using MathContext
   */
  private void test2(TestHarness harness, MathContext mathContext)
  {
    harness.check(((new BigDecimal("2")).abs(mathContext).toString()).equals("2"));
    harness.check(((new BigDecimal("-2")).abs(mathContext).toString()).equals("2"));
    harness.check(((new BigDecimal("+0.000")).abs(mathContext).toString()).equals("0.000"));
    harness.check(((new BigDecimal("00.000")).abs(mathContext).toString()).equals("0.000"));
    harness.check(((new BigDecimal("-0.000")).abs(mathContext).toString()).equals("0.000"));
    harness.check(((new BigDecimal("0.2")).abs(mathContext).toString()).equals("0.2"));
    harness.check(((new BigDecimal("-0.2")).abs(mathContext).toString()).equals("0.2"));
    harness.check(((new BigDecimal("0.01")).abs(mathContext).toString()).equals("0.01"));
    harness.check(((new BigDecimal("-0.01")).abs(mathContext).toString()).equals("0.01"));
  }

  /**
   * Various MathContext usage
   */
  private void test3(TestHarness harness)
  {
    harness.check(((new BigDecimal("-2000000")).abs(new MathContext(0)).toString()).equals("2000000"));
    harness.check(((new BigDecimal("-2000000")).abs(new MathContext(1)).toString()).equals("2E+6"));
    harness.check(((new BigDecimal("-2000000")).abs(new MathContext(2)).toString()).equals("2.0E+6"));
    harness.check(((new BigDecimal("-2000000")).abs(new MathContext(3)).toString()).equals("2.00E+6"));
  }
}

