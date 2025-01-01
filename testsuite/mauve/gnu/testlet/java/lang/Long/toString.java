// Test of Long methods toString() and toString(long).

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

package gnu.testlet.java.lang.Long;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

/**
 * Test of Long methods toString() and toString(long).
 */
public class toString implements Testlet
{
  public void test (TestHarness harness)
  {
    // test of method Long.toString()
    harness.check(new Long(0).toString(), "0");
    harness.check(new Long(-1).toString(), "-1");
    harness.check(new Long(1).toString(), "1");
    harness.check(new Long(127).toString(), "127");
    harness.check(new Long(-128).toString(), "-128");
    harness.check(new Long(Integer.MAX_VALUE).toString(), "2147483647");
    harness.check(new Long(Integer.MIN_VALUE).toString(), "-2147483648");
    harness.check(new Long(Long.MAX_VALUE).toString(), "9223372036854775807");
    harness.check(new Long(Long.MIN_VALUE).toString(), "-9223372036854775808");

    // test of static method Long.toString(long)
    harness.check(Long.toString(0), "0");
    harness.check(Long.toString(-1), "-1");
    harness.check(Long.toString(1), "1");
    harness.check(Long.toString(127), "127");
    harness.check(Long.toString(-128), "-128");
    harness.check(Long.toString(Integer.MAX_VALUE), "2147483647");
    harness.check(Long.toString(Integer.MIN_VALUE), "-2147483648");
    harness.check(Long.toString(Long.MAX_VALUE), "9223372036854775807");
    harness.check(Long.toString(Long.MIN_VALUE), "-9223372036854775808");
  }
}

