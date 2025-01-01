// Test of BigDecimal constructors.

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

// Tags: JDK1.5

package gnu.testlet.java.math.BigDecimal;

import java.math.BigDecimal;
import java.math.MathContext;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

/**
 * Test if instance of BigDecimal class could be created
 * directly from an int value.
 */
public class constructorInt implements Testlet
{
  /**
   * int values used as a parameter passed to constructors.
   */
  private static final int INT_VALUE_1 =  0;
  private static final int INT_VALUE_2 = -0;
  private static final int INT_VALUE_3 = 1;
  private static final int INT_VALUE_4 = -1;
  private static final int INT_VALUE_5 = Integer.MAX_VALUE;
  private static final int INT_VALUE_6 = Integer.MIN_VALUE;

  /**
   * Entry point to this test.
   */
  public void test(TestHarness harness)
  {
    test1(harness);
    test2(harness);
  }

  /**
   * Constructor BigDecimal(int)
   */
  public void test1(TestHarness harness)
  {
    harness.checkPoint("constructor BigDecimal(int)");
    try {
       harness.check(new BigDecimal(INT_VALUE_1).toString (), "0");
       harness.check(new BigDecimal(INT_VALUE_2).toString (), "0");
       harness.check(new BigDecimal(INT_VALUE_3).toString (), "1");
       harness.check(new BigDecimal(INT_VALUE_4).toString (), "-1");
       harness.check(new BigDecimal(INT_VALUE_5).toString (), "2147483647");
       harness.check(new BigDecimal(INT_VALUE_6).toString (), "-2147483648");
    } 
    catch (Exception e) {
       harness.fail("Exception should not be thrown here." + e);
    }
  }

  /**
   * Constructor BigDecimal(int, MathContext)
   */
  public void test2(TestHarness harness)
  {
    harness.checkPoint("constructor BigDecimal(int, MathContext)");
    try {
       harness.check(new BigDecimal(INT_VALUE_1, MathContext.UNLIMITED).toString (), "0");
       harness.check(new BigDecimal(INT_VALUE_2, MathContext.UNLIMITED).toString (), "0");
       harness.check(new BigDecimal(INT_VALUE_3, MathContext.UNLIMITED).toString (), "1");
       harness.check(new BigDecimal(INT_VALUE_4, MathContext.UNLIMITED).toString (), "-1");
       harness.check(new BigDecimal(INT_VALUE_5, MathContext.UNLIMITED).toString (), "2147483647");
       harness.check(new BigDecimal(INT_VALUE_6, MathContext.UNLIMITED).toString (), "-2147483648");
    } 
    catch (Exception e) {
       harness.fail("Exception should not be thrown here." + e);
    }
  }

}

