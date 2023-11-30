// Test of the method BigDecimal.pow()

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
  * Check for method BigDecimal.pow()
  */
public class pow implements Testlet
{
  class TestCase
  {
    public String input;
    public int    power;
    public String output;

    public TestCase(String input, int power, String output) {
      this.input = input;
      this.power = power;
      this.output = output;
    }
  };

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
    TestCase[] testCases = new TestCase[] {
      new TestCase("0", 0, "1"),
      new TestCase("1", 0, "1"),
      new TestCase("0", 1, "0"),
      new TestCase("2", 2, "4"),
      new TestCase("2", 2, "4"),
      new TestCase("2.", 2, "4"),
      new TestCase("2.0", 2, "4.00"),
      new TestCase("2.00", 2, "4.0000"),
      new TestCase("2.000", 2, "4.000000"),

      new TestCase("-0", 0, "1"),
      new TestCase("-1", 0, "1"),
      new TestCase("-0", 1, "0"),
      new TestCase("-2", 2, "4"),
      new TestCase("-2", 2, "4"),
      new TestCase("-2.", 2, "4"),
      new TestCase("-2.0", 2, "4.00"),
      new TestCase("-2.00", 2, "4.0000"),
      new TestCase("-2.000", 2, "4.000000"),
    };
    for (int i = 0; i < testCases.length; i++) {
      TestCase testCase = testCases[i];
      // System.out.println(testCase.input + "\t" + testCase.power + "\t" + new BigDecimal(testCase.input).pow(testCase.power).toString());
      harness.check(((new BigDecimal(testCase.input)).pow(testCase.power).toString()).equals(testCase.output));
    }
  }

  /**
   * Tests using MathContext
   */
  private void test2(TestHarness harness, MathContext mathContext)
  {
    TestCase[] testCases = new TestCase[] {
      new TestCase("0", 0, "1"),
      new TestCase("1", 0, "1"),
      new TestCase("0", 1, "0"),
      new TestCase("2", 2, "4"),
      new TestCase("2", 2, "4"),
      new TestCase("2.", 2, "4"),
      new TestCase("2.0", 2, "4.00"),
      new TestCase("2.00", 2, "4.0000"),
      new TestCase("2.000", 2, "4.000000"),

      new TestCase("-0", 0, "1"),
      new TestCase("-1", 0, "1"),
      new TestCase("-0", 1, "0"),
      new TestCase("-2", 2, "4"),
      new TestCase("-2", 2, "4"),
      new TestCase("-2.", 2, "4"),
      new TestCase("-2.0", 2, "4.00"),
      new TestCase("-2.00", 2, "4.0000"),
      new TestCase("-2.000", 2, "4.000000"),
    };
    for (int i = 0; i < testCases.length; i++) {
      TestCase testCase = testCases[i];
      // System.out.println(testCase.input + "\t" + testCase.power + "\t" + new BigDecimal(testCase.input).pow(testCase.power).toString());
      harness.check(((new BigDecimal(testCase.input)).pow(testCase.power).toString()).equals(testCase.output));
    }
  }

  /**
   * Tests using MathContext
   */
  private void test3(TestHarness harness, MathContext mathContext)
  {
    TestCase[] testCases = new TestCase[] {
      new TestCase("0", 0, "1"),
      new TestCase("1", 0, "1"),
      new TestCase("0", 1, "0"),
      new TestCase("2", 2, "4"),
      new TestCase("2", 2, "4"),
      new TestCase("2.", 2, "4"),
      new TestCase("2.0", 2, "4.00"),
      new TestCase("2.00", 2, "4.0000"),
      new TestCase("2.000", 2, "4.000000"),

      new TestCase("-0", 0, "1"),
      new TestCase("-1", 0, "1"),
      new TestCase("-0", 1, "0"),
      new TestCase("-2", 2, "4"),
      new TestCase("-2", 2, "4"),
      new TestCase("-2.", 2, "4"),
      new TestCase("-2.0", 2, "4.00"),
      new TestCase("-2.00", 2, "4.0000"),
      new TestCase("-2.000", 2, "4.000000"),
    };
    for (int i = 0; i < testCases.length; i++) {
      TestCase testCase = testCases[i];
      // System.out.println(testCase.input + "\t" + testCase.power + "\t" + new BigDecimal(testCase.input).pow(testCase.power).toString());
      harness.check(((new BigDecimal(testCase.input)).pow(testCase.power).toString()).equals(testCase.output));
    }
  }

  /**
   * Various MathContext usage
   */
  private void test4(TestHarness harness)
  {
    harness.check(((new BigDecimal("2000000")).pow(1, new MathContext(0)).toString()).equals("2000000"));
    harness.check(((new BigDecimal("2000000")).pow(-2, new MathContext(1)).toString()).equals("3E-13"));
  }
}

