// Test of the method BigDecimal.toPlainString()

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

package gnu.testlet.java.math.BigDecimal;

import java.math.BigDecimal;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

/**
 * Test the method BigDecimal.toPlainString()
 */
public class toPlainString implements Testlet
{
  /**
   * Entry point to this test.
   */
  public void test(TestHarness harness)
  {
    harness.checkPoint("constructor BigDecimal(BigInteger)");
    try {
       // constant values
       harness.check(BigDecimal.ZERO.toPlainString(), "0");
       harness.check(BigDecimal.ONE.toPlainString(), "1");
       harness.check(BigDecimal.TEN.toPlainString(), "10");

       // simple positive values
       harness.check(new BigDecimal(1).toPlainString(), "1");
       harness.check(new BigDecimal("1").toPlainString(), "1");
       harness.check(new BigDecimal(10).toPlainString(), "10");
       harness.check(new BigDecimal(100).toPlainString(), "100");
       harness.check(new BigDecimal(1000).toPlainString(), "1000");
       harness.check(new BigDecimal(10000).toPlainString(), "10000");
       harness.check(new BigDecimal(100000).toPlainString(), "100000");
       harness.check(new BigDecimal(1).toPlainString(), "1");

       // negative values
       harness.check(new BigDecimal("-1").toPlainString(), "-1");
       harness.check(new BigDecimal(-10).toPlainString(), "-10");
       harness.check(new BigDecimal(-100).toPlainString(), "-100");
       harness.check(new BigDecimal(-1000).toPlainString(), "-1000");
       harness.check(new BigDecimal(-10000).toPlainString(), "-10000");
       harness.check(new BigDecimal(-100000).toPlainString(), "-100000");

       // positive values with positive exponents
       harness.check(new BigDecimal("1e8").toPlainString(),  "100000000");
       harness.check(new BigDecimal("1e9").toPlainString(),  "1000000000");
       harness.check(new BigDecimal("1e10").toPlainString(), "10000000000");
       harness.check(new BigDecimal("1e11").toPlainString(), "100000000000");
       harness.check(new BigDecimal("1e12").toPlainString(), "1000000000000");

       // negative values with positive exponents
       harness.check(new BigDecimal("-1e8").toPlainString(),  "-100000000");
       harness.check(new BigDecimal("-1e9").toPlainString(),  "-1000000000");
       harness.check(new BigDecimal("-1e10").toPlainString(), "-10000000000");
       harness.check(new BigDecimal("-1e11").toPlainString(), "-100000000000");
       harness.check(new BigDecimal("-1e12").toPlainString(), "-1000000000000");

       // positive values with negative exponents
       harness.check(new BigDecimal("1e-8").toPlainString(),  "0.00000001");
       harness.check(new BigDecimal("1e-9").toPlainString(),  "0.000000001");
       harness.check(new BigDecimal("1e-10").toPlainString(), "0.0000000001");
       harness.check(new BigDecimal("1e-11").toPlainString(), "0.00000000001");
       harness.check(new BigDecimal("1e-12").toPlainString(), "0.000000000001");

       // negative values with negative exponents
       harness.check(new BigDecimal("-1e-8").toPlainString(),  "-0.00000001");
       harness.check(new BigDecimal("-1e-9").toPlainString(),  "-0.000000001");
       harness.check(new BigDecimal("-1e-10").toPlainString(), "-0.0000000001");
       harness.check(new BigDecimal("-1e-11").toPlainString(), "-0.00000000001");
       harness.check(new BigDecimal("-1e-12").toPlainString(), "-0.000000000001");
    } 
    catch (Exception e) {
       harness.fail("Exception should not be thrown here." + e);
    }
  }
}

