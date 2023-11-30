// Test the conversion between double and long.

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

package gnu.testlet.java.lang.Double;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

/**
 * Test the conversion between double and long.
 */
public class doubleToLong implements Testlet
{

  /**
   * Entry point to this test.
   */
  public void test(TestHarness harness)
  {
     testZero(harness);
     testPositiveValue(harness);
     testNegativeValue(harness);
     testNaN(harness);
     testPositiveInfinity(harness);
     testNegativeInfinity(harness);
  }

  public void testZero(TestHarness harness)
  {
     double value = 0.0D;
     boolean result = (long)value == 0;
     harness.check(result);
  }

  public void testPositiveValue(TestHarness harness)
  {
     double value = 123.456D;
     boolean result = (long)value == 123;
     harness.check(result);

     value = 456.789D;
     result = (long)value == 456;
     harness.check(result);
  }

  public void testNegativeValue(TestHarness harness)
  {
     double value = -123.456D;
     boolean result = (long)value == -123;
     harness.check(result);

     value = -456.789D;
     result = (long)value == -456;
     harness.check(result);
  }

  public void testNaN(TestHarness harness)
  {
     double value = 0.0D / 0.0D;
     boolean result = (long)value == 0;
     harness.check(result);
  }

  public void testPositiveInfinity(TestHarness harness)
  {
     double value = 100.0D / 0.0D;
     boolean result = (long)value == 0x7fffffffffffffffL;
     harness.check(result);
  }

  public void testNegativeInfinity(TestHarness harness)
  {
     double value = -100.0D / 0.0D;
     boolean result = (long)value == 0x8000000000000000L;
     harness.check(result);
  }
}

