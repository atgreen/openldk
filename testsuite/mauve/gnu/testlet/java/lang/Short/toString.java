// Test of Short methods toString() and toString(short).

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

package gnu.testlet.java.lang.Short;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

/**
 * Test of Short methods toString() and toString(short).
 */
public class toString implements Testlet
{
  public void test (TestHarness harness)
  {
    // test of method Short.toString()
    harness.check(new Short((short)0).toString(), "0");
    harness.check(new Short((short)-1).toString(), "-1");
    harness.check(new Short((short)1).toString(), "1");
    harness.check(new Short((short)127).toString(), "127");
    harness.check(new Short((short)-128).toString(), "-128");
    harness.check(new Short(Short.MIN_VALUE).toString(), "-32768");
    harness.check(new Short(Short.MAX_VALUE).toString(), "32767");

    // test of static method Short.toString(Short)
    harness.check(Short.toString((short)0), "0");
    harness.check(Short.toString((short)-1), "-1");
    harness.check(Short.toString((short)1), "1");
    harness.check(Short.toString((short)127), "127");
    harness.check(Short.toString((short)-128), "-128");
    harness.check(Short.toString(Short.MIN_VALUE), "-32768");
    harness.check(Short.toString(Short.MAX_VALUE), "32767");
  }
}

