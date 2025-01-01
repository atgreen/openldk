// Test of the method BigDecimal.shortValue()

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
  * Check for method BigDecimal.shortValue()
  */
public class shortValue implements Testlet
{
  /**
   * Entry point to this test.
   */
  public void test(TestHarness harness)
  {
    // simple conversions
    harness.check((0)      == (new BigDecimal("0")).shortValue());
    harness.check((1)      == (new BigDecimal("1")).shortValue());
    harness.check((99)     == (new BigDecimal("99")).shortValue());
    harness.check((100)    == (new BigDecimal("100")).shortValue());
    harness.check((127)    == (new BigDecimal("127")).shortValue());
    harness.check((-127)   == (new BigDecimal("-127")).shortValue());
    harness.check((128)    == (new BigDecimal("128")).shortValue());
    harness.check((-128)   == (new BigDecimal("-128")).shortValue());
    harness.check((32767)  == (new BigDecimal("32767")).shortValue());
    harness.check((-32768) == (new BigDecimal("-32768")).shortValue());
    harness.check((-32768) == (new BigDecimal("32768")).shortValue());

    // overflows/underflows
    harness.check((-1)  == ((new BigDecimal("65535")).shortValue()));
    harness.check((0)   == ((new BigDecimal("65536")).shortValue()));
    harness.check((-1)  == ((new BigDecimal("131071")).shortValue()));
    harness.check((0)   == ((new BigDecimal("131072")).shortValue()));

    // truncation
    harness.check((100) == (new BigDecimal("100.0")).shortValue());
    harness.check((100) == (new BigDecimal("100.1")).shortValue());
    harness.check((100) == (new BigDecimal("100.9")).shortValue());
  }

}

