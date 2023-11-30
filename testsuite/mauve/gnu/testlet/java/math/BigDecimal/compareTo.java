// Tags: JDK1.1

// Copyright (C) 2006 Free Software Foundation, Inc.
// Contributed by Mark Wielaard (mark@klomp.org)
 
// This file is part of Mauve.
 
// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
 
// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
 
// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

package gnu.testlet.java.math.BigDecimal;

import java.math.BigDecimal;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class compareTo implements Testlet
{
  public void test (TestHarness harness)
  {
    BigDecimal a, b;
    a = new BigDecimal("0.1");
    b = new BigDecimal("0.01");
    harness.check(a.compareTo(b), 1);
    harness.check(b.compareTo(a), -1);

    a = new BigDecimal("10.1");
    b = new BigDecimal("10.01");
    harness.check(a.compareTo(b), 1);
    harness.check(b.compareTo(a), -1);

    a = new BigDecimal("10.10");
    b = new BigDecimal("10.01");
    harness.check(a.compareTo(b), 1);
    harness.check(b.compareTo(a), -1);

    a = new BigDecimal("10.10");
    b = new BigDecimal("10.010");
    harness.check(a.compareTo(b), 1);
    harness.check(b.compareTo(a), -1);

    a = new BigDecimal("10.100");
    b = new BigDecimal("10.010");
    harness.check(a.compareTo(b), 1);
    harness.check(b.compareTo(a), -1);

    a = new BigDecimal("10.100");
    b = new BigDecimal("10.01");
    harness.check(a.compareTo(b), 1);
    harness.check(b.compareTo(a), -1);

    a = new BigDecimal("010.100");
    b = new BigDecimal("10.01");
    harness.check(a.compareTo(b), 1);
    harness.check(b.compareTo(a), -1);

    a = new BigDecimal("010.100");
    b = new BigDecimal("010.01");
    harness.check(a.compareTo(b), 1);
    harness.check(b.compareTo(a), -1);

    a = new BigDecimal("10.100");
    b = new BigDecimal("010.01");
    harness.check(a.compareTo(b), 1);
    harness.check(b.compareTo(a), -1);

    a = new BigDecimal("0.10");
    b = new BigDecimal("0.10");
    harness.check(a.compareTo(b), 0);
    harness.check(b.compareTo(a), 0);

    a = new BigDecimal("0.1");
    b = new BigDecimal("0.10");
    harness.check(a.compareTo(b), 0);
    harness.check(b.compareTo(a), 0);

    a = new BigDecimal("0.1");
    b = new BigDecimal("0.100");
    harness.check(a.compareTo(b), 0);
    harness.check(b.compareTo(a), 0);

    a = new BigDecimal("0.10");
    b = new BigDecimal("0.100");
    harness.check(a.compareTo(b), 0);
    harness.check(b.compareTo(a), 0);

    a = new BigDecimal("10.10");
    b = new BigDecimal("10.10");
    harness.check(a.compareTo(b), 0);
    harness.check(b.compareTo(a), 0);

    a = new BigDecimal("10.100");
    b = new BigDecimal("10.10");
    harness.check(a.compareTo(b), 0);
    harness.check(b.compareTo(a), 0);

    a = new BigDecimal("10.101");
    b = new BigDecimal("10.10");
    harness.check(a.compareTo(b), 1);
    harness.check(b.compareTo(a), -1);

    a = new BigDecimal("10.001");
    b = new BigDecimal("10.10");
    harness.check(a.compareTo(b), -1);
    harness.check(b.compareTo(a), 1);

    a = new BigDecimal("10.001");
    b = new BigDecimal("10.010");
    harness.check(a.compareTo(b), -1);
    harness.check(b.compareTo(a), 1);

    a = new BigDecimal("10.0010");
    b = new BigDecimal("10.010");
    harness.check(a.compareTo(b), -1);
    harness.check(b.compareTo(a), 1);

    a = new BigDecimal("10.0010");
    b = new BigDecimal("10.0100");
    harness.check(a.compareTo(b), -1);
    harness.check(b.compareTo(a), 1);

    a = new BigDecimal("10.0010");
    b = new BigDecimal("10.01000");
    harness.check(a.compareTo(b), -1);
    harness.check(b.compareTo(a), 1);
  }
}
