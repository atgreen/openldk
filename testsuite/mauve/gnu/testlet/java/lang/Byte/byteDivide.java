// Test byte division operation.

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

package gnu.testlet.java.lang.Byte;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

/**
 * Test integer division operation.
 */
public class byteDivide implements Testlet
{

  /**
   * Entry pobyte to this test.
   */
  public void test(TestHarness harness)
  {
     testDividePositiveByPositiveCase1(harness);
     testDividePositiveByPositiveCase2(harness);
     testDividePositiveByPositiveCase3(harness);
     testDividePositiveByNegativeCase1(harness);
     testDividePositiveByNegativeCase2(harness);
     testDividePositiveByNegativeCase3(harness);
     testDivideNegativeByPositiveCase1(harness);
     testDivideNegativeByPositiveCase2(harness);
     testDivideNegativeByPositiveCase3(harness);
     testDivideNegativeByNegativeCase1(harness);
     testDivideNegativeByNegativeCase2(harness);
     testDivideNegativeByNegativeCase3(harness);
     testDivideMaxValue(harness);
     testDivideMinValue(harness);
     testDivideByMaxValue(harness);
     testDivideByMinValue(harness);
     testDivideByZeroCase1(harness);
     testDivideByZeroCase2(harness);
     testDivideByZeroCase3(harness);
     testDivideByZeroCase4(harness);
     testDivideByZeroCase5(harness);
  }

  public void testDividePositiveByPositiveCase1(TestHarness harness)
  {
    byte x = 10;
    byte y = 2;
    byte z = (byte)(x / y);
    harness.check(z == 5);
  }

  public void testDividePositiveByPositiveCase2(TestHarness harness)
  {
    byte x = 10;
    byte y = 3;
    byte z = (byte)(x / y);
    harness.check(z == 3);
  }

  public void testDividePositiveByPositiveCase3(TestHarness harness)
  {
    byte x = 11;
    byte y = 3;
    byte z = (byte)(x / y);
    harness.check(z == 3);
  }

  public void testDividePositiveByNegativeCase1(TestHarness harness)
  {
    byte x = 10;
    byte y = -2;
    byte z = (byte)(x / y);
    harness.check(z == -5);
  }

  public void testDividePositiveByNegativeCase2(TestHarness harness)
  {
    byte x = 10;
    byte y = -3;
    byte z = (byte)(x / y);
    harness.check(z == -3);
  }

  public void testDividePositiveByNegativeCase3(TestHarness harness)
  {
    byte x = 11;
    byte y = -3;
    byte z = (byte)(x / y);
    harness.check(z == -3);
  }

  public void testDivideNegativeByPositiveCase1(TestHarness harness)
  {
    byte x = -10;
    byte y = 2;
    byte z = (byte)(x / y);
    harness.check(z == -5);
  }

  public void testDivideNegativeByPositiveCase2(TestHarness harness)
  {
    byte x = -10;
    byte y = 3;
    byte z = (byte)(x / y);
    harness.check(z == -3);
  }

  public void testDivideNegativeByPositiveCase3(TestHarness harness)
  {
    byte x = -11;
    byte y = 3;
    byte z = (byte)(x / y);
    harness.check(z == -3);
  }

  public void testDivideNegativeByNegativeCase1(TestHarness harness)
  {
    byte x = -10;
    byte y = -2;
    byte z = (byte)(x / y);
    harness.check(z == 5);
  }

  public void testDivideNegativeByNegativeCase2(TestHarness harness)
  {
    byte x = -10;
    byte y = -3;
    byte z = (byte)(x / y);
    harness.check(z == 3);
  }

  public void testDivideNegativeByNegativeCase3(TestHarness harness)
  {
    byte x = -11;
    byte y = -3;
    byte z = (byte)(x / y);
    harness.check(z == 3);
  }

  public void testDivideMaxValue(TestHarness harness)
  {
    byte x = Byte.MAX_VALUE;
    byte y = 1;
    byte z = (byte)(x / y);
    harness.check(z == 127);
  }

  public void testDivideMinValue(TestHarness harness)
  {
    byte x = Byte.MIN_VALUE;
    byte y = 1;
    byte z = (byte)(x / y);
    harness.check(z == -128);
  }

  public void testDivideByMaxValue(TestHarness harness)
  {
    byte x = Byte.MAX_VALUE;
    byte y = Byte.MAX_VALUE;
    byte z = (byte)(x / y);
    harness.check(z == 1);
  }

  public void testDivideByMinValue(TestHarness harness)
  {
    byte x = Byte.MIN_VALUE;
    byte y = Byte.MIN_VALUE;
    byte z = (byte)(x / y);
    harness.check(z == 1);
  }

  public void testDivideByZeroCase1(TestHarness harness)
  {
    byte x = 1;
    byte y = 0;
    try { 
        byte z = (byte)(x / y);
        harness.check(false);
    }
    catch(ArithmeticException e) { 
        harness.check(true);
    }
  }

  public void testDivideByZeroCase2(TestHarness harness)
  {
    byte x = -1;
    byte y = 0;
    try { 
        byte z = (byte)(x / y);
        harness.check(false);
    }
    catch(ArithmeticException e) { 
        harness.check(true);
    }
  }

  public void testDivideByZeroCase3(TestHarness harness)
  {
    byte x = Byte.MAX_VALUE;
    byte y = 0;
    try { 
        byte z = (byte)(x / y);
        harness.check(false);
    }
    catch(ArithmeticException e) { 
        harness.check(true);
    }
  }

  public void testDivideByZeroCase4(TestHarness harness)
  {
    byte x = Byte.MIN_VALUE;
    byte y = 0;
    try { 
        byte z = (byte)(x / y);
        harness.check(false);
    }
    catch(ArithmeticException e) { 
        harness.check(true);
    }
  }

  public void testDivideByZeroCase5(TestHarness harness)
  {
    byte x = 0;
    byte y = 0;
    try { 
        byte z = (byte)(x / y);
        harness.check(false);
    }
    catch(ArithmeticException e) { 
        harness.check(true);
    }
  }

}

