// Test of Byte method valueOf(byte).

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

package gnu.testlet.java.lang.Byte;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

/**
 * Test the method Byte.valueOf(byte).
 */
public class valueOfByte implements Testlet
{
  public void test (TestHarness harness)
  {
    harness.check(new Byte((byte)0).equals(Byte.valueOf((byte)0)));
    harness.check(new Byte((byte)-1).equals(Byte.valueOf((byte)-1)));
    harness.check(new Byte((byte)1).equals(Byte.valueOf((byte)1)));
    harness.check(new Byte((byte)127).equals(Byte.valueOf((byte)127)));
    harness.check(new Byte((byte)-128).equals(Byte.valueOf((byte)-128)));
  }
}

