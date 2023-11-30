// Test of Boolean methods parseBoolean().

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

// Tags: JDK1.4
// Tags: CompileOptions: -source 1.4

package gnu.testlet.java.lang.Boolean;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

/**
 * Test of Boolean methods parseBoolean().
 */
public class parseBoolean implements Testlet
{
  public void test (TestHarness harness)
  {
    // test of static method Boolean.parseBoolean()
    harness.check(Boolean.parseBoolean("true"), true);
    harness.check(Boolean.parseBoolean("false"), false);
    harness.check(Boolean.parseBoolean("TRUE"), true);
    harness.check(Boolean.parseBoolean("FALSE"), false);
    harness.check(Boolean.parseBoolean("True"), true);
    harness.check(Boolean.parseBoolean("False"), false);
    harness.check(Boolean.parseBoolean("truE"), true);
    harness.check(Boolean.parseBoolean("falsE"), false);
    harness.check(Boolean.parseBoolean(" true"), false);
    harness.check(Boolean.parseBoolean(" false"), false);
    harness.check(Boolean.parseBoolean(" true "), false);
    harness.check(Boolean.parseBoolean(" false "), false);
  }
}


