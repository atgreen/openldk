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
// We use `1.5' because the constructor with parametes char[] is first
// defined here.

package gnu.testlet.java.math.BigDecimal;

import java.math.BigDecimal;
import java.math.MathContext;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

/**
 * Test if instance of BigDecimal class could be created
 * directly from character sequence.
 */
public class constructorCharacterSequence implements Testlet
{
  /**
   * Sequence of characters used as a parameter passed to constructors.
   */
  private static final char[] CHAR_SEQUENCE_0 = "0.0".toCharArray();
  private static final char[] CHAR_SEQUENCE_1 = "0.1".toCharArray();
  private static final char[] CHAR_SEQUENCE_2 = "0.01".toCharArray();
  private static final char[] CHAR_SEQUENCE_3 = "0.001".toCharArray();
  private static final char[] CHAR_SEQUENCE_4 = "0.0001".toCharArray();
  private static final char[] CHAR_SEQUENCE_5 = "0.00001".toCharArray();
  private static final char[] CHAR_SEQUENCE_6 = "0.000001".toCharArray();
  private static final char[] CHAR_SEQUENCE_7 = "0.0000001".toCharArray();
  private static final char[] CHAR_SEQUENCE_8 = "0.01E5".toCharArray();
  private static final char[] CHAR_SEQUENCE_9 = "1000E-5".toCharArray();

  /**
   * This character sequence is used by constructors which accepts
   * character subsequence.
   */
  private static final char[] CHAR_SEQUENCE_10 = "123456".toCharArray();

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
   * Constructor BigDecimal(char[])
   */
  public void test1(TestHarness harness)
  {
    harness.checkPoint("constructor BigDecimal(char[])");
    try {
       harness.check(new BigDecimal(CHAR_SEQUENCE_0).toString (), "0.0");
       harness.check(new BigDecimal(CHAR_SEQUENCE_1).toString (), "0.1");
       harness.check(new BigDecimal(CHAR_SEQUENCE_2).toString (), "0.01");
       harness.check(new BigDecimal(CHAR_SEQUENCE_3).toString (), "0.001");
       harness.check(new BigDecimal(CHAR_SEQUENCE_4).toString (), "0.0001");
       harness.check(new BigDecimal(CHAR_SEQUENCE_5).toString (), "0.00001");
       harness.check(new BigDecimal(CHAR_SEQUENCE_6).toString (), "0.000001");
       harness.check(new BigDecimal(CHAR_SEQUENCE_7).toString (), "1E-7");
       harness.check(new BigDecimal(CHAR_SEQUENCE_8).toString (), "1E+3");
       harness.check(new BigDecimal(CHAR_SEQUENCE_9).toString (), "0.01000");
    } 
    catch (Exception e) {
       harness.fail("Exception should not be thrown here." + e);
    }
  }

  /**
   * Constructor BigDecimal(char[], offset, length)
   */
  public void test2(TestHarness harness)
  {
    harness.checkPoint("constructor BigDecimal(char[], offset, length)");
    harness.check(new BigDecimal(CHAR_SEQUENCE_10, 0, 1).toString(), "1");
    harness.check(new BigDecimal(CHAR_SEQUENCE_10, 0, 2).toString(), "12");
    harness.check(new BigDecimal(CHAR_SEQUENCE_10, 1, 1).toString(), "2");
    harness.check(new BigDecimal(CHAR_SEQUENCE_10, 1, 2).toString(), "23");

    try {
      System.out.println(new BigDecimal(CHAR_SEQUENCE_10, 1, 10).toString());
      harness.fail("Exception don't thrown as expected");
    }
    catch (NumberFormatException e) {
      // it is expected, that exception is thrown
    }
    try {
      System.out.println(new BigDecimal(CHAR_SEQUENCE_10, -1, 2).toString());
      harness.fail("Exception don't thrown as expected");
    }
    catch (NumberFormatException e) {
      // it is expected, that exception is thrown
    }
  }

  /**
   * Constructor BigDecimal(char[], MathContext)
   */
  public void test3(TestHarness harness)
  {
    harness.checkPoint("constructor BigDecimal(char[], MathContext)");
    try {
       harness.check(new BigDecimal(CHAR_SEQUENCE_0, MathContext.UNLIMITED).toString (), "0.0");
       harness.check(new BigDecimal(CHAR_SEQUENCE_1, MathContext.UNLIMITED).toString (), "0.1");
       harness.check(new BigDecimal(CHAR_SEQUENCE_2, MathContext.UNLIMITED).toString (), "0.01");
       harness.check(new BigDecimal(CHAR_SEQUENCE_3, MathContext.UNLIMITED).toString (), "0.001");
       harness.check(new BigDecimal(CHAR_SEQUENCE_4, MathContext.UNLIMITED).toString (), "0.0001");
       harness.check(new BigDecimal(CHAR_SEQUENCE_5, MathContext.UNLIMITED).toString (), "0.00001");
       harness.check(new BigDecimal(CHAR_SEQUENCE_6, MathContext.UNLIMITED).toString (), "0.000001");
       harness.check(new BigDecimal(CHAR_SEQUENCE_7, MathContext.UNLIMITED).toString (), "1E-7");
       harness.check(new BigDecimal(CHAR_SEQUENCE_8, MathContext.UNLIMITED).toString (), "1E+3");
       harness.check(new BigDecimal(CHAR_SEQUENCE_9, MathContext.UNLIMITED).toString (), "0.01000");
    } 
    catch (Exception e) {
       harness.fail("Exception should not be thrown here." + e);
    }
  }

  /**
   * Constructor BigDecimal(char[], offset, length, MathContext)
   */
  public void test4(TestHarness harness)
  {
    harness.checkPoint("constructor BigDecimal(char[], offset, length, MathContext)");
    harness.check(new BigDecimal(CHAR_SEQUENCE_10, 0, 1, MathContext.UNLIMITED).toString(), "1");
    harness.check(new BigDecimal(CHAR_SEQUENCE_10, 0, 2, MathContext.UNLIMITED).toString(), "12");
    harness.check(new BigDecimal(CHAR_SEQUENCE_10, 1, 1, MathContext.UNLIMITED).toString(), "2");
    harness.check(new BigDecimal(CHAR_SEQUENCE_10, 1, 2, MathContext.UNLIMITED).toString(), "23");

    try {
      System.out.println(new BigDecimal(CHAR_SEQUENCE_10, 1, 10).toString());
      harness.fail("Exception don't thrown as expected");
    }
    catch (NumberFormatException e) {
      // it is expected, that exception is thrown
    }
    try {
      System.out.println(new BigDecimal(CHAR_SEQUENCE_10, -1, 2).toString());
      harness.fail("Exception don't thrown as expected");
    }
    catch (NumberFormatException e) {
      // it is expected, that exception is thrown
    }
  }

}

