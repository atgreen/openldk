// Test of Double method doubleToLongBits(double).

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
 * Test the method Double.doubleToLongBits(double);
 */
public class doubleToLongBits implements Testlet
{

  /**
   * Entry point to this test.
   */
  public void test(TestHarness harness)
  {
     harness.check(0x0000000000000000L, Double.doubleToLongBits(0));
     harness.check(0x3ff0000000000000L, Double.doubleToLongBits(1));
     harness.check(0x4000000000000000L, Double.doubleToLongBits(2));
     harness.check(0x408f400000000000L, Double.doubleToLongBits(1000));
     harness.check(0xbff0000000000000L, Double.doubleToLongBits(-1));
     harness.check(0xc000000000000000L, Double.doubleToLongBits(-2));
     harness.check(0xc08f400000000000L, Double.doubleToLongBits(-1000));
     harness.check(0x7ff0000000000000L, Double.doubleToLongBits(Double.POSITIVE_INFINITY));
     harness.check(0xfff0000000000000L, Double.doubleToLongBits(Double.NEGATIVE_INFINITY));
     harness.check(0x7fefffffffffffffL, Double.doubleToLongBits(Double.MAX_VALUE));
     harness.check(0x0000000000000001L, Double.doubleToLongBits(Double.MIN_VALUE));
     harness.check(0xffefffffffffffffL, Double.doubleToLongBits(-Double.MAX_VALUE));
     harness.check(0x8000000000000001L, Double.doubleToLongBits(-Double.MIN_VALUE));
     harness.check(0x7ff8000000000000L, Double.doubleToLongBits(Double.NaN));
  }
}

