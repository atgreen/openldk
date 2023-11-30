// Test of Boolean method valueOf(String)

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
// Tags: CompileOptions: -source 1.5

package gnu.testlet.java.lang.Boolean;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

/**
 * Test the method Boolean.valueOf(String).
 */
public class valueOfString implements Testlet
{
  public void test (TestHarness harness)
  {
    harness.check(Boolean.valueOf("true"), Boolean.TRUE);
    harness.check(Boolean.valueOf("false"), Boolean.FALSE);
    harness.check(Boolean.valueOf("TRUE"), Boolean.TRUE);
    harness.check(Boolean.valueOf("FALSE"), Boolean.FALSE);
    harness.check(Boolean.valueOf("tRUE"), Boolean.TRUE);
    harness.check(Boolean.valueOf("fALSE"), Boolean.FALSE);
    harness.check(Boolean.valueOf("tRUe"), Boolean.TRUE);
    harness.check(Boolean.valueOf("fALSe"), Boolean.FALSE);
  }
}

