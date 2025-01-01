// Test of BigDecimal method compareTo(Object).

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
// Tags: CompileOptions: -source 1.4

package gnu.testlet.java.math.BigDecimal;

import java.math.BigDecimal;
import java.math.BigInteger;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

/**
 * Test the method BigDecimal.compareTo(Object);
 * Partially based on test compareTo(BigDecimal)
 */
public class compareToObject implements Testlet
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
   * Positive test for method BigDecimal.compareTo(Object)
   */
  public void positiveTest(TestHarness harness)
  {
    harness.checkPoint("positive test");
    try {
      BigDecimal a, b;
      a = new BigDecimal("0.1");
      b = new BigDecimal("0.01");
      harness.check(compare(a,b), 1);
      harness.check(compare(b,a), -1);

      a = new BigDecimal("10.1");
      b = new BigDecimal("10.01");
      harness.check(compare(a,b), 1);
      harness.check(compare(b,a), -1);

      a = new BigDecimal("10.10");
      b = new BigDecimal("10.01");
      harness.check(compare(a,b), 1);
      harness.check(compare(b,a), -1);

      a = new BigDecimal("10.10");
      b = new BigDecimal("10.010");
      harness.check(compare(a,b), 1);
      harness.check(compare(b,a), -1);

      a = new BigDecimal("10.100");
      b = new BigDecimal("10.010");
      harness.check(compare(a,b), 1);
      harness.check(compare(b,a), -1);

      a = new BigDecimal("10.100");
      b = new BigDecimal("10.01");
      harness.check(compare(a,b), 1);
      harness.check(compare(b,a), -1);

      a = new BigDecimal("010.100");
      b = new BigDecimal("10.01");
      harness.check(compare(a,b), 1);
      harness.check(compare(b,a), -1);

      a = new BigDecimal("010.100");
      b = new BigDecimal("010.01");
      harness.check(compare(a,b), 1);
      harness.check(compare(b,a), -1);

      a = new BigDecimal("10.100");
      b = new BigDecimal("010.01");
      harness.check(compare(a,b), 1);
      harness.check(compare(b,a), -1);

      a = new BigDecimal("0.10");
      b = new BigDecimal("0.10");
      harness.check(compare(a,b), 0);
      harness.check(compare(b,a), 0);

      a = new BigDecimal("0.1");
      b = new BigDecimal("0.10");
      harness.check(compare(a,b), 0);
      harness.check(compare(b,a), 0);

      a = new BigDecimal("0.1");
      b = new BigDecimal("0.100");
      harness.check(compare(a,b), 0);
      harness.check(compare(b,a), 0);

      a = new BigDecimal("0.10");
      b = new BigDecimal("0.100");
      harness.check(compare(a,b), 0);
      harness.check(compare(b,a), 0);

      a = new BigDecimal("10.10");
      b = new BigDecimal("10.10");
      harness.check(compare(a,b), 0);
      harness.check(compare(b,a), 0);

      a = new BigDecimal("10.100");
      b = new BigDecimal("10.10");
      harness.check(compare(a,b), 0);
      harness.check(compare(b,a), 0);

      a = new BigDecimal("10.101");
      b = new BigDecimal("10.10");
      harness.check(compare(a,b), 1);
      harness.check(compare(b,a), -1);

      a = new BigDecimal("10.001");
      b = new BigDecimal("10.10");
      harness.check(compare(a,b), -1);
      harness.check(compare(b,a), 1);

      a = new BigDecimal("10.001");
      b = new BigDecimal("10.010");
      harness.check(compare(a,b), -1);
      harness.check(compare(b,a), 1);

      a = new BigDecimal("10.0010");
      b = new BigDecimal("10.010");
      harness.check(compare(a,b), -1);
      harness.check(compare(b,a), 1);

      a = new BigDecimal("10.0010");
      b = new BigDecimal("10.0100");
      harness.check(compare(a,b), -1);
      harness.check(compare(b,a), 1);

      a = new BigDecimal("10.0010");
      b = new BigDecimal("10.01000");
      harness.check(compare(a,b), -1);
      harness.check(compare(b,a), 1);
    }
    catch (ClassCastException e) {
      harness.fail("Exception should not be thrown here." + e);
    }
  }

  /**
   * Negative test for method BigDecimal.compareTo(Object)
   */
  public void negativeTest(TestHarness harness)
  {
    harness.checkPoint("negative test");
    try {
      BigDecimal a;
      Byte b;
      a = new BigDecimal("0.1");
      b = new Byte((byte)1);
      compare(a,b);
      harness.fail("Exception should be thrown here.");
    } 
    catch (ClassCastException e) {
      // ok - exception was thrown
      harness.check(true);
    }
    try {
      BigDecimal a;
      String b;
      a = new BigDecimal("0.1");
      b = "foobar";
      compare(a,b);
      harness.fail("Exception should be thrown here.");
    } 
    catch (ClassCastException e) {
      // ok - exception was thrown
      harness.check(true);
    }
  }

  private int compare(BigDecimal a, Object b) throws ClassCastException
  {
    return a.compareTo(b);
  }
}

