// Test of the method BigDecimal.toEngineeringString()

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
 * Test the method BigDecimal.toEngineeringString()
 */
public class toEngineeringString implements Testlet
{
  /**
   * Entry point to this test.
   */
  public void test(TestHarness harness)
  {
    harness.checkPoint("constructor BigDecimal(BigInteger)");
    try {
       // constant values
       harness.check(BigDecimal.ZERO.toEngineeringString(), "0");
       harness.check(BigDecimal.ONE.toEngineeringString(), "1");
       harness.check(BigDecimal.TEN.toEngineeringString(), "10");

       // simple positive values
       harness.check(new BigDecimal(1).toEngineeringString(), "1");
       harness.check(new BigDecimal("1").toEngineeringString(), "1");
       harness.check(new BigDecimal(10).toEngineeringString(), "10");
       harness.check(new BigDecimal(100).toEngineeringString(), "100");
       harness.check(new BigDecimal(1000).toEngineeringString(), "1000");
       harness.check(new BigDecimal(10000).toEngineeringString(), "10000");
       harness.check(new BigDecimal(100000).toEngineeringString(), "100000");
       harness.check(new BigDecimal(1).toEngineeringString(), "1");

       // negative values
       harness.check(new BigDecimal("-1").toEngineeringString(), "-1");
       harness.check(new BigDecimal(-10).toEngineeringString(), "-10");
       harness.check(new BigDecimal(-100).toEngineeringString(), "-100");
       harness.check(new BigDecimal(-1000).toEngineeringString(), "-1000");
       harness.check(new BigDecimal(-10000).toEngineeringString(), "-10000");
       harness.check(new BigDecimal(-100000).toEngineeringString(), "-100000");

       // positive values with positive exponents
       harness.check(new BigDecimal("1e8").toEngineeringString(), "100E+6");
       harness.check(new BigDecimal("1e9").toEngineeringString(), "1E+9");
       harness.check(new BigDecimal("1e10").toEngineeringString(), "10E+9");
       harness.check(new BigDecimal("1e11").toEngineeringString(), "100E+9");
       harness.check(new BigDecimal("1e12").toEngineeringString(), "1E+12");

       // negative values with positive exponents
       harness.check(new BigDecimal("-1e8").toEngineeringString(), "-100E+6");
       harness.check(new BigDecimal("-1e9").toEngineeringString(), "-1E+9");
       harness.check(new BigDecimal("-1e10").toEngineeringString(), "-10E+9");
       harness.check(new BigDecimal("-1e11").toEngineeringString(), "-100E+9");
       harness.check(new BigDecimal("-1e12").toEngineeringString(), "-1E+12");

       // positive values with negative exponents
       harness.check(new BigDecimal("1e-8").toEngineeringString(), "10E-9");
       harness.check(new BigDecimal("1e-9").toEngineeringString(), "1E-9");
       harness.check(new BigDecimal("1e-10").toEngineeringString(), "100E-12");
       harness.check(new BigDecimal("1e-11").toEngineeringString(), "10E-12");
       harness.check(new BigDecimal("1e-12").toEngineeringString(), "1E-12");

       // negative values with negative exponents
       harness.check(new BigDecimal("-1e-8").toEngineeringString(), "-10E-9");
       harness.check(new BigDecimal("-1e-9").toEngineeringString(), "-1E-9");
       harness.check(new BigDecimal("-1e-10").toEngineeringString(), "-100E-12");
       harness.check(new BigDecimal("-1e-11").toEngineeringString(), "-10E-12");
       harness.check(new BigDecimal("-1e-12").toEngineeringString(), "-1E-12");
    } 
    catch (Exception e) {
       harness.fail("Exception should not be thrown here." + e);
    }
  }
}

