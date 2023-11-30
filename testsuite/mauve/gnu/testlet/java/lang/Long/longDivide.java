// Test long division operation.

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

package gnu.testlet.java.lang.Long;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

/**
 * Test long division operation.
 */
public class longDivide implements Testlet
{

  /**
   * Entry point to this test.
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
    long x = 10L;
    long y = 2L;
    long z = x / y;
    harness.check(z == 5);
  }

  public void testDividePositiveByPositiveCase2(TestHarness harness)
  {
    long x = 10L;
    long y = 3L;
    long z = x / y;
    harness.check(z == 3);
  }

  public void testDividePositiveByPositiveCase3(TestHarness harness)
  {
    long x = 11L;
    long y = 3L;
    long z = x / y;
    harness.check(z == 3);
  }

  public void testDividePositiveByNegativeCase1(TestHarness harness)
  {
    long x = 10L;
    long y = -2L;
    long z = x / y;
    harness.check(z == -5);
  }

  public void testDividePositiveByNegativeCase2(TestHarness harness)
  {
    long x = 10L;
    long y = -3L;
    long z = x / y;
    harness.check(z == -3);
  }

  public void testDividePositiveByNegativeCase3(TestHarness harness)
  {
    long x = 11L;
    long y = -3L;
    long z = x / y;
    harness.check(z == -3);
  }

  public void testDivideNegativeByPositiveCase1(TestHarness harness)
  {
    long x = -10L;
    long y = 2L;
    long z = x / y;
    harness.check(z == -5);
  }

  public void testDivideNegativeByPositiveCase2(TestHarness harness)
  {
    long x = -10L;
    long y = 3L;
    long z = x / y;
    harness.check(z == -3);
  }

  public void testDivideNegativeByPositiveCase3(TestHarness harness)
  {
    long x = -11L;
    long y = 3L;
    long z = x / y;
    harness.check(z == -3);
  }

  public void testDivideNegativeByNegativeCase1(TestHarness harness)
  {
    long x = -10L;
    long y = -2L;
    long z = x / y;
    harness.check(z == 5);
  }

  public void testDivideNegativeByNegativeCase2(TestHarness harness)
  {
    long x = -10L;
    long y = -3L;
    long z = x / y;
    harness.check(z == 3);
  }

  public void testDivideNegativeByNegativeCase3(TestHarness harness)
  {
    long x = -11L;
    long y = -3L;
    long z = x / y;
    harness.check(z == 3);
  }

  public void testDivideMaxValue(TestHarness harness)
  {
    long x = Integer.MAX_VALUE;
    long y = 2 << 15L;
    long z = x / y;
    harness.check(z == 32767);
  }

  public void testDivideMinValue(TestHarness harness)
  {
    long x = Integer.MIN_VALUE;
    long y = 2 << 15L;
    long z = x / y;
    harness.check(z == -32768);
  }

  public void testDivideByMaxValue(TestHarness harness)
  {
    long x = Integer.MAX_VALUE;
    long y = Integer.MAX_VALUE;
    long z = x / y;
    harness.check(z == 1);
  }

  public void testDivideByMinValue(TestHarness harness)
  {
    long x = Integer.MIN_VALUE;
    long y = Integer.MIN_VALUE;
    long z = x / y;
    harness.check(z == 1);
  }

  public void testDivideByZeroCase1(TestHarness harness)
  {
    long x = 1L;
    long y = 0L;
    try { 
        long z = x / y;
        harness.check(false);
    }
    catch(ArithmeticException e) { 
        harness.check(true);
    }
  }

  public void testDivideByZeroCase2(TestHarness harness)
  {
    long x = -1L;
    long y = 0L;
    try { 
        long z = x / y;
        harness.check(false);
    }
    catch(ArithmeticException e) { 
        harness.check(true);
    }
  }

  public void testDivideByZeroCase3(TestHarness harness)
  {
    long x = Integer.MAX_VALUE;
    long y = 0L;
    try { 
        long z = x / y;
        harness.check(false);
    }
    catch(ArithmeticException e) { 
        harness.check(true);
    }
  }

  public void testDivideByZeroCase4(TestHarness harness)
  {
    long x = Integer.MIN_VALUE;
    long y = 0L;
    try { 
        long z = x / y;
        harness.check(false);
    }
    catch(ArithmeticException e) { 
        harness.check(true);
    }
  }

  public void testDivideByZeroCase5(TestHarness harness)
  {
    long x = 0L;
    long y = 0L;
    try { 
        long z = x / y;
        harness.check(false);
    }
    catch(ArithmeticException e) { 
        harness.check(true);
    }
  }

}

