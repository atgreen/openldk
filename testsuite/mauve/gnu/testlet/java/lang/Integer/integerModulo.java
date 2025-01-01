// Test integer modulo operation.

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

// Tags: JDK1.0

package gnu.testlet.java.lang.Integer;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

/**
 * Test integer modulo operation.
 */
public class integerModulo implements Testlet
{

  /**
   * Entry point to this test.
   */
  public void test(TestHarness harness)
  {
     testModuloPositiveByPositiveCase1(harness);
     testModuloPositiveByPositiveCase2(harness);
     testModuloPositiveByPositiveCase3(harness);
     testModuloPositiveByNegativeCase1(harness);
     testModuloPositiveByNegativeCase2(harness);
     testModuloPositiveByNegativeCase3(harness);
     testModuloNegativeByPositiveCase1(harness);
     testModuloNegativeByPositiveCase2(harness);
     testModuloNegativeByPositiveCase3(harness);
     testModuloNegativeByNegativeCase1(harness);
     testModuloNegativeByNegativeCase2(harness);
     testModuloNegativeByNegativeCase3(harness);
     testModuloMaxValue(harness);
     testModuloMinValue(harness);
     testModuloByMaxValue(harness);
     testModuloByMinValue(harness);
     testModuloByZeroCase1(harness);
     testModuloByZeroCase2(harness);
     testModuloByZeroCase3(harness);
     testModuloByZeroCase4(harness);
     testModuloByZeroCase5(harness);
  }

  public void testModuloPositiveByPositiveCase1(TestHarness harness)
  {
    int x = 10;
    int y = 2;
    int z = x % y;
    harness.check(z == 0);
  }

  public void testModuloPositiveByPositiveCase2(TestHarness harness)
  {
    int x = 10;
    int y = 3;
    int z = x % y;
    harness.check(z == 1);
  }

  public void testModuloPositiveByPositiveCase3(TestHarness harness)
  {
    int x = 11;
    int y = 3;
    int z = x % y;
    harness.check(z == 2);
  }

  public void testModuloPositiveByNegativeCase1(TestHarness harness)
  {
    int x = 10;
    int y = -2;
    int z = x % y;
    harness.check(z == 0);
  }

  public void testModuloPositiveByNegativeCase2(TestHarness harness)
  {
    int x = 10;
    int y = -3;
    int z = x % y;
    harness.check(z == 1);
  }

  public void testModuloPositiveByNegativeCase3(TestHarness harness)
  {
    int x = 11;
    int y = -3;
    int z = x % y;
    harness.check(z == 2);
  }

  public void testModuloNegativeByPositiveCase1(TestHarness harness)
  {
    int x = -10;
    int y = 2;
    int z = x % y;
    harness.check(z == 0);
  }

  public void testModuloNegativeByPositiveCase2(TestHarness harness)
  {
    int x = -10;
    int y = 3;
    int z = x % y;
    harness.check(z == -1);
  }

  public void testModuloNegativeByPositiveCase3(TestHarness harness)
  {
    int x = -11;
    int y = 3;
    int z = x % y;
    harness.check(z == -2);
  }

  public void testModuloNegativeByNegativeCase1(TestHarness harness)
  {
    int x = -10;
    int y = -2;
    int z = x % y;
    harness.check(z == 0);
  }

  public void testModuloNegativeByNegativeCase2(TestHarness harness)
  {
    int x = -10;
    int y = -3;
    int z = x % y;
    harness.check(z == -1);
  }

  public void testModuloNegativeByNegativeCase3(TestHarness harness)
  {
    int x = -11;
    int y = -3;
    int z = x % y;
    harness.check(z == -2);
  }

  public void testModuloMaxValue(TestHarness harness)
  {
    int x = Integer.MAX_VALUE;
    int y = 2 << 15;
    int z = x % y;
    harness.check(z == 65535);
  }

  public void testModuloMinValue(TestHarness harness)
  {
    int x = Integer.MIN_VALUE;
    int y = 2 << 15;
    int z = x % y;
    harness.check(z == 0);
  }

  public void testModuloByMaxValue(TestHarness harness)
  {
    int x = Integer.MAX_VALUE;
    int y = Integer.MAX_VALUE;
    int z = x % y;
    harness.check(z == 0);
  }

  public void testModuloByMinValue(TestHarness harness)
  {
    int x = Integer.MIN_VALUE;
    int y = Integer.MIN_VALUE;
    int z = x % y;
    harness.check(z == 0);
  }

  public void testModuloByZeroCase1(TestHarness harness)
  {
    int x = 1;
    int y = 0;
    try { 
        int z = x % y;
        harness.check(false);
    }
    catch(ArithmeticException e) { 
        harness.check(true);
    }
  }

  public void testModuloByZeroCase2(TestHarness harness)
  {
    int x = -1;
    int y = 0;
    try { 
        int z = x % y;
        harness.check(false);
    }
    catch(ArithmeticException e) { 
        harness.check(true);
    }
  }

  public void testModuloByZeroCase3(TestHarness harness)
  {
    int x = Integer.MAX_VALUE;
    int y = 0;
    try { 
        int z = x % y;
        harness.check(false);
    }
    catch(ArithmeticException e) { 
        harness.check(true);
    }
  }

  public void testModuloByZeroCase4(TestHarness harness)
  {
    int x = Integer.MIN_VALUE;
    int y = 0;
    try { 
        int z = x % y;
        harness.check(false);
    }
    catch(ArithmeticException e) { 
        harness.check(true);
    }
  }

  public void testModuloByZeroCase5(TestHarness harness)
  {
    int x = 0;
    int y = 0;
    try { 
        int z = x % y;
        harness.check(false);
    }
    catch(ArithmeticException e) { 
        harness.check(true);
    }
  }

}

