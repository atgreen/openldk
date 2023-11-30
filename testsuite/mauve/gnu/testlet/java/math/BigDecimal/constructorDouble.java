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
 * directly from a double value.
 */
public class constructorDouble implements Testlet
{
  /**
   * Double values used as a parameter passed to constructors.
   */
  private static final double DOUBLE_VALUE_1 =  .0;
  private static final double DOUBLE_VALUE_2 = -.0;
  private static final double DOUBLE_VALUE_3 = 10.0;
  private static final double DOUBLE_VALUE_4 = -10.0;
  private static final double DOUBLE_VALUE_5 = 10000000000.0;
  private static final double DOUBLE_VALUE_6 = -10000000000.0;

  /**
   * Converting following values should cause NumberFormatException to be thrown.
   */
  private static final double DOUBLE_VALUE_NAN = Double.NaN;

  /**
   * Entry point to this test.
   */
  public void test(TestHarness harness)
  {
    test1(harness);
    test2(harness);
  }

  /**
   * Constructor BigDecimal(double)
   */
  public void test1(TestHarness harness)
  {
    harness.checkPoint("constructor BigDecimal(double)");
    try {
       harness.check(new BigDecimal(DOUBLE_VALUE_1).toString (), "0");
       harness.check(new BigDecimal(DOUBLE_VALUE_2).toString (), "0");
       harness.check(new BigDecimal(DOUBLE_VALUE_3).toString (), "10");
       harness.check(new BigDecimal(DOUBLE_VALUE_4).toString (), "-10");
       harness.check(new BigDecimal(DOUBLE_VALUE_5).toString (), "10000000000");
       harness.check(new BigDecimal(DOUBLE_VALUE_6).toString (), "-10000000000");
    } 
    catch (Exception e) {
       harness.fail("Exception should not be thrown here." + e);
    }
    try {
        harness.check(new BigDecimal(DOUBLE_VALUE_NAN).toString(), "");
        harness.fail("NumberFormatException not thrown as expected");
    }
    catch (NumberFormatException e) {
        // ok, we assume that this exception occured here
    }
  }

  /**
   * Constructor BigDecimal(double, MathContext)
   */
  public void test2(TestHarness harness)
  {
    harness.checkPoint("constructor BigDecimal(double, MathContext)");
    try {
       harness.check(new BigDecimal(DOUBLE_VALUE_1, MathContext.UNLIMITED).toString (), "0");
       harness.check(new BigDecimal(DOUBLE_VALUE_2, MathContext.UNLIMITED).toString (), "0");
       harness.check(new BigDecimal(DOUBLE_VALUE_3, MathContext.UNLIMITED).toString (), "10");
       harness.check(new BigDecimal(DOUBLE_VALUE_4, MathContext.UNLIMITED).toString (), "-10");
       harness.check(new BigDecimal(DOUBLE_VALUE_5, MathContext.UNLIMITED).toString (), "10000000000");
       harness.check(new BigDecimal(DOUBLE_VALUE_6, MathContext.UNLIMITED).toString (), "-10000000000");
    } 
    catch (Exception e) {
       harness.fail("Exception should not be thrown here." + e);
    }
    try {
        harness.check(new BigDecimal(DOUBLE_VALUE_NAN, MathContext.UNLIMITED).toString(), "");
        harness.fail("NumberFormatException not thrown as expected");
    }
    catch (NumberFormatException e) {
        // ok, we assume that this exception occured here
    }
  }

}

