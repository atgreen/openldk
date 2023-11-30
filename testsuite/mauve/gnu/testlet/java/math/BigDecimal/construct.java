// Test of BigDecimal constructors.

// Copyright 2001 Red Hat, Inc.
// Written by Tom Tromey <tromey@redhat.com>

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

// Tags: JDK1.3
// We use `1.3' because we have tests involving exponential notation.

package gnu.testlet.java.math.BigDecimal;

import java.math.BigDecimal;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class construct implements Testlet
{
  public void test (TestHarness harness)
  {
    harness.check(new BigDecimal ("0.1").toString (), "0.1");
    // This example comes from the documentation.
    harness.check(new BigDecimal (0.1).toString (),
		  "0.1000000000000000055511151231257827021181583404541015625");
    
    try {
       harness.check(new BigDecimal ("0.01E5").toString (), "1E+3");
       harness.check(new BigDecimal ("1000E-5").toString (), "0.01000");
    } 
    catch (Exception e) {
       harness.fail("Exception should not be thrown here." + e);
    }
    // Add more as needed.
  }
}
