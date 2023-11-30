// Test of the method BigDecimal.multiply()

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
  * Check for method BigDecimal.multiply()
  */
public class multiply implements Testlet
{
  /**
   * Entry point to this test.
   */
  public void test(TestHarness harness)
  {
    test1(harness);
    test2(harness, new MathContext(0));
    test3(harness, new MathContext(1));
    test4(harness);
  }

  /**
   * Basic tests
   */
  private void test1(TestHarness harness)
  {
    BigDecimal secondNumber = new BigDecimal("2");
    harness.check(((new BigDecimal("2")).multiply(secondNumber).toString()).equals("4"));
    harness.check(((new BigDecimal("-2")).multiply(secondNumber).toString()).equals("-4"));
    harness.check(((new BigDecimal("+0.000")).multiply(secondNumber).toString()).equals("0.000"));
    harness.check(((new BigDecimal("00.000")).multiply(secondNumber).toString()).equals("0.000"));
    harness.check(((new BigDecimal("-0.000")).multiply(secondNumber).toString()).equals("0.000"));
    harness.check(((new BigDecimal("2000000")).multiply(secondNumber).toString()).equals("4000000"));
    harness.check(((new BigDecimal("0.2")).multiply(secondNumber).toString()).equals("0.4"));
    harness.check(((new BigDecimal("-0.2")).multiply(secondNumber).toString()).equals("-0.4"));
    harness.check(((new BigDecimal("0.01")).multiply(secondNumber).toString()).equals("0.02"));
    harness.check(((new BigDecimal("-0.01")).multiply(secondNumber).toString()).equals("-0.02"));
  }

  /**
   * Tests using MathContext
   */
  private void test2(TestHarness harness, MathContext mathContext)
  {
    BigDecimal secondNumber = new BigDecimal("2");
    harness.check(((new BigDecimal("2")).multiply(secondNumber, mathContext).toString()).equals("4"));
    harness.check(((new BigDecimal("-2")).multiply(secondNumber, mathContext).toString()).equals("-4"));
    harness.check(((new BigDecimal("+0.000")).multiply(secondNumber, mathContext).toString()).equals("0.000"));
    harness.check(((new BigDecimal("00.000")).multiply(secondNumber, mathContext).toString()).equals("0.000"));
    harness.check(((new BigDecimal("-0.000")).multiply(secondNumber, mathContext).toString()).equals("0.000"));
    harness.check(((new BigDecimal("2000000")).multiply(secondNumber, mathContext).toString()).equals("4000000"));
    harness.check(((new BigDecimal("0.2")).multiply(secondNumber, mathContext).toString()).equals("0.4"));
    harness.check(((new BigDecimal("-0.2")).multiply(secondNumber, mathContext).toString()).equals("-0.4"));
    harness.check(((new BigDecimal("0.01")).multiply(secondNumber, mathContext).toString()).equals("0.02"));
    harness.check(((new BigDecimal("-0.01")).multiply(secondNumber, mathContext).toString()).equals("-0.02"));
  }

  /**
   * Tests using MathContext
   */
  private void test3(TestHarness harness, MathContext mathContext)
  {
    BigDecimal secondNumber = new BigDecimal("2");
    harness.check(((new BigDecimal("2")).multiply(secondNumber, mathContext).toString()).equals("4"));
    harness.check(((new BigDecimal("-2")).multiply(secondNumber, mathContext).toString()).equals("-4"));
    harness.check(((new BigDecimal("+0.000")).multiply(secondNumber, mathContext).toString()).equals("0.000"));
    harness.check(((new BigDecimal("00.000")).multiply(secondNumber, mathContext).toString()).equals("0.000"));
    harness.check(((new BigDecimal("-0.000")).multiply(secondNumber, mathContext).toString()).equals("0.000"));
    harness.check(((new BigDecimal("2000000")).multiply(secondNumber, mathContext).toString()).equals("4E+6"));
    harness.check(((new BigDecimal("0.2")).multiply(secondNumber, mathContext).toString()).equals("0.4"));
    harness.check(((new BigDecimal("-0.2")).multiply(secondNumber, mathContext).toString()).equals("-0.4"));
    harness.check(((new BigDecimal("0.01")).multiply(secondNumber, mathContext).toString()).equals("0.02"));
    harness.check(((new BigDecimal("-0.01")).multiply(secondNumber, mathContext).toString()).equals("-0.02"));
  }

  /**
   * Various MathContext usage
   */
  private void test4(TestHarness harness)
  {
    BigDecimal secondNumber = new BigDecimal("2");
    harness.check(((new BigDecimal("2000000")).multiply(secondNumber, new MathContext(0)).toString()).equals("4000000"));
    harness.check(((new BigDecimal("2000000")).multiply(secondNumber, new MathContext(1)).toString()).equals("4E+6"));
    harness.check(((new BigDecimal("2000000")).multiply(secondNumber, new MathContext(2)).toString()).equals("4.0E+6"));
    harness.check(((new BigDecimal("2000000")).multiply(secondNumber, new MathContext(3)).toString()).equals("4.00E+6"));
  }
}

