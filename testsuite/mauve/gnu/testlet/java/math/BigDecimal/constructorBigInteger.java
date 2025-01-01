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
import java.math.BigInteger;
import java.math.MathContext;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

/**
 * Test if instance of BigDecimal class could be created
 * directly from an BigInteger value.
 */
public class constructorBigInteger implements Testlet
{
  /**
   * Big Integer values used as a parameter passed to constructors.
   */
  private static final BigInteger BIGINT_ZERO = BigInteger.ZERO;
  private static final BigInteger BIGINT_ONE = BigInteger.valueOf (1);
  private static final BigInteger BIGINT_TEN = BigInteger.TEN;
  private static final BigInteger BIGINT_M_ONE = BigInteger.valueOf (-1);
  private static final BigInteger BIGINT_LONG_VALUE = new BigInteger("123456789000");

  /**
   * Entry point to this test.
   */
  public void test(TestHarness harness)
  {
    test1(harness);
    test2(harness);
    test3(harness);
    test4(harness);
  }

  /**
   * Constructor BigDecimal(BigInteger)
   */
  public void test1(TestHarness harness)
  {
    harness.checkPoint("constructor BigDecimal(BigInteger)");
    try {
       harness.check(new BigDecimal(BIGINT_ZERO).toString (), "0");
       harness.check(new BigDecimal(BIGINT_ONE).toString (), "1");
       harness.check(new BigDecimal(BIGINT_TEN).toString (), "10");
       harness.check(new BigDecimal(BIGINT_M_ONE).toString (), "-1");
       harness.check(new BigDecimal(BIGINT_LONG_VALUE).toString (), "123456789000");
    } 
    catch (Exception e) {
       harness.fail("Exception should not be thrown here." + e);
    }
  }

  /**
   * Constructor BigDecimal(BigInteger, MathContext)
   */
  public void test2(TestHarness harness)
  {
    harness.checkPoint("constructor BigDecimal(BigInteger, MathContext)");
    try {
       harness.check(new BigDecimal(BIGINT_ZERO, MathContext.UNLIMITED).toString (), "0");
       harness.check(new BigDecimal(BIGINT_ONE, MathContext.UNLIMITED).toString (), "1");
       harness.check(new BigDecimal(BIGINT_TEN, MathContext.UNLIMITED).toString (), "10");
       harness.check(new BigDecimal(BIGINT_M_ONE, MathContext.UNLIMITED).toString (), "-1");
       harness.check(new BigDecimal(BIGINT_LONG_VALUE, MathContext.UNLIMITED).toString (), "123456789000");
    } 
    catch (Exception e) {
       harness.fail("Exception should not be thrown here." + e);
    }
  }

  /**
   * Constructor BigDecimal(BigInteger, scale)
   */
  public void test3(TestHarness harness)
  {
    harness.checkPoint("constructor BigDecimal(BigInteger, scale)");
    try {
       harness.check(new BigDecimal(BIGINT_ONE, 0).toString (), "1");
       harness.check(new BigDecimal(BIGINT_ONE, 1).toString (), "0.1");
       harness.check(new BigDecimal(BIGINT_ONE, 4).toString (), "0.0001");
       harness.check(new BigDecimal(BIGINT_ONE, 10).toString (), "1E-10");
       harness.check(new BigDecimal(BIGINT_ONE, -1).toString (), "1E+1");
       harness.check(new BigDecimal(BIGINT_ONE, -2).toString (), "1E+2");
    } 
    catch (Exception e) {
       harness.fail("Exception should not be thrown here." + e);
    }
  }

  /**
   * Constructor BigDecimal(BigInteger, scale)
   */
  public void test4(TestHarness harness)
  {
    harness.checkPoint("constructor BigDecimal(BigInteger, scale, MathContext)");
    try {
       harness.check(new BigDecimal(BIGINT_ONE, 0, MathContext.UNLIMITED).toString (), "1");
       harness.check(new BigDecimal(BIGINT_ONE, 1, MathContext.UNLIMITED).toString (), "0.1");
       harness.check(new BigDecimal(BIGINT_ONE, 4, MathContext.UNLIMITED).toString (), "0.0001");
       harness.check(new BigDecimal(BIGINT_ONE, 10, MathContext.UNLIMITED).toString (), "1E-10");
       harness.check(new BigDecimal(BIGINT_ONE, -1, MathContext.UNLIMITED).toString (), "1E+1");
       harness.check(new BigDecimal(BIGINT_ONE, -2, MathContext.UNLIMITED).toString (), "1E+2");
    } 
    catch (Exception e) {
       harness.fail("Exception should not be thrown here." + e);
    }
  }

}

