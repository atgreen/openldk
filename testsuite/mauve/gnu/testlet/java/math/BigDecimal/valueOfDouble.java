// Test of the method BigDecimal.valueOf(double)

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
//
// Tags: JDK1.5

package gnu.testlet.java.math.BigDecimal;

import java.math.BigDecimal;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

/**
  * Check for method BigDecimal.valueOf(double)
  */
public class valueOfDouble implements Testlet
{
  /**
   * Entry point to this test.
   */
  public void test(TestHarness harness)
  {
    // simple conversions
    harness.check(new BigDecimal("0.0"), BigDecimal.valueOf(0.0));
    harness.check(new BigDecimal("1.0"), BigDecimal.valueOf(1.0));
    harness.check(new BigDecimal("0.0"), BigDecimal.valueOf(0.));
    harness.check(new BigDecimal("0.0"), BigDecimal.valueOf(.0));
    harness.check(new BigDecimal("1.0"), BigDecimal.valueOf(1.));
    harness.check(new BigDecimal("1.7976931348623157E+308"), BigDecimal.valueOf(Double.MAX_VALUE));
    harness.check(new BigDecimal("4.9E-324"), BigDecimal.valueOf(Double.MIN_VALUE));

    try {
      BigDecimal.valueOf(Double.NaN);
      harness.fail("Exception don't thrown as expected");
    }
    catch (NumberFormatException e) {
      // ok, we expected this exception
    }
  }

}

