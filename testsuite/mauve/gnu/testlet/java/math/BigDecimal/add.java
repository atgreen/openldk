// Test of the method BigDecimal.add()

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
  * Check for method BigDecimal.add()
  */
public class add implements Testlet
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
    harness.check(((new BigDecimal("2")).add(secondNumber).toString()).equals("4"));
    harness.check(((new BigDecimal("-2")).add(secondNumber).toString()).equals("0"));
    harness.check(((new BigDecimal("+0.000")).add(secondNumber).toString()).equals("2.000"));
    harness.check(((new BigDecimal("00.000")).add(secondNumber).toString()).equals("2.000"));
    harness.check(((new BigDecimal("-0.000")).add(secondNumber).toString()).equals("2.000"));
    harness.check(((new BigDecimal("2000000")).add(secondNumber).toString()).equals("2000002"));
    harness.check(((new BigDecimal("0.2")).add(secondNumber).toString()).equals("2.2"));
    harness.check(((new BigDecimal("-0.2")).add(secondNumber).toString()).equals("1.8"));
    harness.check(((new BigDecimal("0.01")).add(secondNumber).toString()).equals("2.01"));
    harness.check(((new BigDecimal("-0.01")).add(secondNumber).toString()).equals("1.99"));
  }

  /**
   * Tests using MathContext
   */
  private void test2(TestHarness harness, MathContext mathContext)
  {
    BigDecimal secondNumber = new BigDecimal("2");
    harness.check(((new BigDecimal("2")).add(secondNumber, mathContext).toString()).equals("4"));
    harness.check(((new BigDecimal("-2")).add(secondNumber, mathContext).toString()).equals("0"));
    harness.check(((new BigDecimal("+0.000")).add(secondNumber, mathContext).toString()).equals("2.000"));
    harness.check(((new BigDecimal("00.000")).add(secondNumber, mathContext).toString()).equals("2.000"));
    harness.check(((new BigDecimal("-0.000")).add(secondNumber, mathContext).toString()).equals("2.000"));
    harness.check(((new BigDecimal("2000000")).add(secondNumber, mathContext).toString()).equals("2000002"));
    harness.check(((new BigDecimal("0.2")).add(secondNumber, mathContext).toString()).equals("2.2"));
    harness.check(((new BigDecimal("-0.2")).add(secondNumber, mathContext).toString()).equals("1.8"));
    harness.check(((new BigDecimal("0.01")).add(secondNumber, mathContext).toString()).equals("2.01"));
    harness.check(((new BigDecimal("-0.01")).add(secondNumber, mathContext).toString()).equals("1.99"));
  }

  /**
   * Tests using MathContext
   */
  private void test3(TestHarness harness, MathContext mathContext)
  {
    BigDecimal secondNumber = new BigDecimal("2");
    harness.check(((new BigDecimal("2")).add(secondNumber, mathContext).toString()).equals("4"));
    harness.check(((new BigDecimal("-2")).add(secondNumber, mathContext).toString()).equals("0"));
    harness.check(((new BigDecimal("+0.000")).add(secondNumber, mathContext).toString()).equals("2"));
    harness.check(((new BigDecimal("00.000")).add(secondNumber, mathContext).toString()).equals("2"));
    harness.check(((new BigDecimal("-0.000")).add(secondNumber, mathContext).toString()).equals("2"));
    harness.check(((new BigDecimal("2000000")).add(secondNumber, mathContext).toString()).equals("2E+6"));
    harness.check(((new BigDecimal("0.2")).add(secondNumber, mathContext).toString()).equals("2"));
    harness.check(((new BigDecimal("-0.2")).add(secondNumber, mathContext).toString()).equals("2"));
    harness.check(((new BigDecimal("0.01")).add(secondNumber, mathContext).toString()).equals("2"));
    harness.check(((new BigDecimal("-0.01")).add(secondNumber, mathContext).toString()).equals("2"));
  }

  /**
   * Various MathContext usage
   */
  private void test4(TestHarness harness)
  {
    BigDecimal secondNumber = new BigDecimal("2");
    harness.check(((new BigDecimal("2000000")).add(secondNumber, new MathContext(0)).toString()).equals("2000002"));
    harness.check(((new BigDecimal("2000000")).add(secondNumber, new MathContext(1)).toString()).equals("2E+6"));
    harness.check(((new BigDecimal("2000000")).add(secondNumber, new MathContext(2)).toString()).equals("2.0E+6"));
    harness.check(((new BigDecimal("2000000")).add(secondNumber, new MathContext(3)).toString()).equals("2.00E+6"));
  }
}

