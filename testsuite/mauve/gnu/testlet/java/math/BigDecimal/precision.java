// Test of BigDecimal method precision().

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
import java.math.BigInteger;
import java.math.MathContext;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

/**
 * Test the method BigDecimal.precision().
 */
public class precision implements Testlet
{
  /**
   * Entry point to this test.
   */
  public void test(TestHarness harness)
  {
      harness.check(new BigDecimal(0).precision(), 1);
      harness.check(BigDecimal.ZERO.precision(), 1);
      harness.check(BigDecimal.ONE.precision(), 1);
      harness.check(BigDecimal.TEN.precision(), 2);
      harness.check(new BigDecimal("1").precision(), 1);
      harness.check(new BigDecimal("1.").precision(), 1);
      harness.check(new BigDecimal("0.1").precision(), 1);
      harness.check(new BigDecimal(".1").precision(), 1);
      harness.check(new BigDecimal("10").precision(), 2);
      harness.check(new BigDecimal("100").precision(), 3);
      harness.check(new BigDecimal("1000").precision(), 4);
      harness.check(new BigDecimal("0.001").precision(), 1);
      harness.check(new BigDecimal(".001").precision(), 1);
      harness.check(new BigDecimal("1.1").precision(), 2);
      harness.check(new BigDecimal("1.01").precision(), 3);
      harness.check(new BigDecimal("1.001").precision(), 4);
      harness.check(new BigDecimal("10.001").precision(), 5);
      harness.check(new BigDecimal("100.001").precision(), 6);
  }

}

