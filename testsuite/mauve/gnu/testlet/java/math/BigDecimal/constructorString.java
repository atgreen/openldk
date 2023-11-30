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
 * directly from a string.
 */
public class constructorString implements Testlet
{
  /**
   * Strings used as a parameter passed to constructors.
   */
  private static final String STRING_LITERAL_0 = "0.0";
  private static final String STRING_LITERAL_1 = "0.1";
  private static final String STRING_LITERAL_2 = "0.01";
  private static final String STRING_LITERAL_3 = "0.001";
  private static final String STRING_LITERAL_4 = "0.0001";
  private static final String STRING_LITERAL_5 = "0.00001";
  private static final String STRING_LITERAL_6 = "0.000001";
  private static final String STRING_LITERAL_7 = "0.0000001";
  private static final String STRING_LITERAL_8 = "0.01E5";
  private static final String STRING_LITERAL_9 = "1000E-5";

  /**
   * Strings used by negative tests.
   */
  private static final String NEG_STRING_LITERAL_0 = "obviously not-a-number";
  private static final String NEG_STRING_LITERAL_1 = "--0";
  private static final String NEG_STRING_LITERAL_2 = "123qwe";
  private static final String NEG_STRING_LITERAL_3 = "qwe123";
  private static final String NEG_STRING_LITERAL_4 = "0.0.0";
  private static final String NEG_STRING_LITERAL_5 = "-0.0.0";

  /**
   * Entry point to this test.
   */
  public void test(TestHarness harness)
  {
    // positive tests
    test1(harness);
    test2(harness);
    // negative tests
    test3(harness);
    test4(harness);
  }

  /**
   * Constructor BigDecimal(String)
   */
  public void test1(TestHarness harness)
  {
    harness.checkPoint("constructor BigDecimal(String)");
    try {
       harness.check(new BigDecimal(STRING_LITERAL_0).toString (), "0.0");
       harness.check(new BigDecimal(STRING_LITERAL_1).toString (), "0.1");
       harness.check(new BigDecimal(STRING_LITERAL_2).toString (), "0.01");
       harness.check(new BigDecimal(STRING_LITERAL_3).toString (), "0.001");
       harness.check(new BigDecimal(STRING_LITERAL_4).toString (), "0.0001");
       harness.check(new BigDecimal(STRING_LITERAL_5).toString (), "0.00001");
       harness.check(new BigDecimal(STRING_LITERAL_6).toString (), "0.000001");
       harness.check(new BigDecimal(STRING_LITERAL_7).toString (), "1E-7");
       harness.check(new BigDecimal(STRING_LITERAL_8).toString (), "1E+3");
       harness.check(new BigDecimal(STRING_LITERAL_9).toString (), "0.01000");
    } 
    catch (Exception e) {
       harness.fail("Exception should not be thrown here." + e);
    }
  }

  /**
   * Constructor BigDecimal(String, MathContext)
   */
  public void test2(TestHarness harness)
  {
    harness.checkPoint("constructor BigDecimal(String, MathContext)");
    try {
       harness.check(new BigDecimal(STRING_LITERAL_0, MathContext.UNLIMITED).toString (), "0.0");
       harness.check(new BigDecimal(STRING_LITERAL_1, MathContext.UNLIMITED).toString (), "0.1");
       harness.check(new BigDecimal(STRING_LITERAL_2, MathContext.UNLIMITED).toString (), "0.01");
       harness.check(new BigDecimal(STRING_LITERAL_3, MathContext.UNLIMITED).toString (), "0.001");
       harness.check(new BigDecimal(STRING_LITERAL_4, MathContext.UNLIMITED).toString (), "0.0001");
       harness.check(new BigDecimal(STRING_LITERAL_5, MathContext.UNLIMITED).toString (), "0.00001");
       harness.check(new BigDecimal(STRING_LITERAL_6, MathContext.UNLIMITED).toString (), "0.000001");
       harness.check(new BigDecimal(STRING_LITERAL_7, MathContext.UNLIMITED).toString (), "1E-7");
       harness.check(new BigDecimal(STRING_LITERAL_8, MathContext.UNLIMITED).toString (), "1E+3");
       harness.check(new BigDecimal(STRING_LITERAL_9, MathContext.UNLIMITED).toString (), "0.01000");
    } 
    catch (Exception e) {
       harness.fail("Exception should not be thrown here." + e);
    }
  }

  /**
   * Constructor BigDecimal(String) - negative tests
   */
  public void test3(TestHarness harness)
  {
    harness.checkPoint("constructor BigDecimal(String) - negative tests");

    try {
       harness.check(new BigDecimal(NEG_STRING_LITERAL_0).toString (), "");
       harness.fail("NumberFormatException not thrown as expected");
    }
    catch (NumberFormatException e) {
       // ok, this should happen
    }

    try {
       harness.check(new BigDecimal(NEG_STRING_LITERAL_1).toString (), "");
       harness.fail("NumberFormatException not thrown as expected");
    }
    catch (NumberFormatException e) {
       // ok, this should happen
    }

    try {
       harness.check(new BigDecimal(NEG_STRING_LITERAL_2).toString (), "");
       harness.fail("NumberFormatException not thrown as expected");
    }
    catch (NumberFormatException e) {
       // ok, this should happen
    }

    try {
       harness.check(new BigDecimal(NEG_STRING_LITERAL_3).toString (), "");
       harness.fail("NumberFormatException not thrown as expected");
    }
    catch (NumberFormatException e) {
       // ok, this should happen
    }

    try {
       harness.check(new BigDecimal(NEG_STRING_LITERAL_4).toString (), "");
       harness.fail("NumberFormatException not thrown as expected");
    }
    catch (NumberFormatException e) {
       // ok, this should happen
    }

    try {
       harness.check(new BigDecimal(NEG_STRING_LITERAL_5).toString (), "");
       harness.fail("NumberFormatException not thrown as expected");
    }
    catch (NumberFormatException e) {
       // ok, this should happen
    }
  }

  /**
   * Constructor BigDecimal(String) - negative tests
   */
  public void test4(TestHarness harness)
  {
    harness.checkPoint("constructor BigDecimal(String, MathContext) - negative tests");

    try {
       harness.check(new BigDecimal(NEG_STRING_LITERAL_0, MathContext.UNLIMITED).toString (), "");
       harness.fail("NumberFormatException not thrown as expected");
    }
    catch (NumberFormatException e) {
       // ok, this should happen
    }

    try {
       harness.check(new BigDecimal(NEG_STRING_LITERAL_1, MathContext.UNLIMITED).toString (), "");
       harness.fail("NumberFormatException not thrown as expected");
    }
    catch (NumberFormatException e) {
       // ok, this should happen
    }

    try {
       harness.check(new BigDecimal(NEG_STRING_LITERAL_2, MathContext.UNLIMITED).toString (), "");
       harness.fail("NumberFormatException not thrown as expected");
    }
    catch (NumberFormatException e) {
       // ok, this should happen
    }

    try {
       harness.check(new BigDecimal(NEG_STRING_LITERAL_3, MathContext.UNLIMITED).toString (), "");
       harness.fail("NumberFormatException not thrown as expected");
    }
    catch (NumberFormatException e) {
       // ok, this should happen
    }

    try {
       harness.check(new BigDecimal(NEG_STRING_LITERAL_4, MathContext.UNLIMITED).toString (), "");
       harness.fail("NumberFormatException not thrown as expected");
    }
    catch (NumberFormatException e) {
       // ok, this should happen
    }

    try {
       harness.check(new BigDecimal(NEG_STRING_LITERAL_5, MathContext.UNLIMITED).toString (), "");
       harness.fail("NumberFormatException not thrown as expected");
    }
    catch (NumberFormatException e) {
       // ok, this should happen
    }

  }
}

