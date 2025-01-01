// Test of the method BigDecimal.longValue()

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
  * Check for method BigDecimal.longValue()
  */
public class longValue implements Testlet
{
  /**
   * Entry point to this test.
   */
  public void test(TestHarness harness)
  {
    // simple conversions
    harness.check((0)           == (new BigDecimal("0")).longValue());
    harness.check((1)           == (new BigDecimal("1")).longValue());
    harness.check((99)          == (new BigDecimal("99")).longValue());
    harness.check((100)         == (new BigDecimal("100")).longValue());
    harness.check((127)         == (new BigDecimal("127")).longValue());
    harness.check((-127)        == (new BigDecimal("-127")).longValue());
    harness.check((128)         == (new BigDecimal("128")).longValue());
    harness.check((-128)        == (new BigDecimal("-128")).longValue());
    harness.check((32767)       == (new BigDecimal("32767")).longValue());
    harness.check((-32768)      == (new BigDecimal("-32768")).longValue());
    harness.check((2147483647)  == ((new BigDecimal("2147483647")).longValue()));
    harness.check((-2147483647) == ((new BigDecimal("-2147483647")).longValue()));

    // overflows/underflows
    harness.check((4294967296L) == ((new BigDecimal("4294967296")).longValue()));
    harness.check((4294967295L) == ((new BigDecimal("4294967295")).longValue()));
    harness.check((4294967294L) == ((new BigDecimal("4294967294")).longValue()));

    // truncation
    harness.check((100) == (new BigDecimal("100.0")).longValue());
    harness.check((100) == (new BigDecimal("100.1")).longValue());
    harness.check((100) == (new BigDecimal("100.9")).longValue());
  }

}

