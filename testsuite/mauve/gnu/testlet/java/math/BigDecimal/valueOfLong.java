// Test of the method BigDecimal.valueOf(long)

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

package gnu.testlet.java.math.BigDecimal;

import java.math.BigDecimal;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

/**
  * Check for method BigDecimal.valueOf(long)
  */
public class valueOfLong implements Testlet
{
  /**
   * Entry point to this test.
   */
  public void test(TestHarness harness)
  {
    // simple conversions
    harness.check(new BigDecimal("0"), BigDecimal.valueOf(0L));
    harness.check(new BigDecimal("1"), BigDecimal.valueOf(1L));
    harness.check(new BigDecimal("9223372036854775807"), BigDecimal.valueOf(Long.MAX_VALUE));
    harness.check(new BigDecimal("-9223372036854775808"), BigDecimal.valueOf(Long.MIN_VALUE));
  }

}

