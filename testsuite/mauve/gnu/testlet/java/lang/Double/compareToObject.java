// Test of Double method compareTo(Object).

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
// Tags: CompileOptions: -source 1.4 -target 1.4

package gnu.testlet.java.lang.Double;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

/**
 * Test the method Double.compareTo(Object);
 */
public class compareToObject implements Testlet
{
  public static final int LESS = -1;
  public static final int EQUAL = 0;
  public static final int GREATER = 1;
  TestHarness harness;
  void compare(Double i1, Object o, int expected)
  {
    // the result need not be -1, 0, 1; just <0, 0, >0
    int result = i1.compareTo(o);
    switch (expected)
      {
      case LESS:
	harness.check(result < 0);
	break;
      case EQUAL:
	harness.check(result == 0);
	break;
      case GREATER:
	harness.check(result > 0);
	break;
      default:
	throw new Error();
      }
  }

  /**
   * Entry point to this test.
   */
  public void test(TestHarness harness)
  {
    this.harness = harness;

    Object o = new Double("0.0");
    
    harness.check(((Comparable)new Double("0.0")).compareTo(o) == 0);

    boolean ok = false;
    try
      {
	new Double("0.0").compareTo((Double) null);
      }
    catch (NullPointerException e)
      {
	ok = true;
      }
    harness.check(ok);

    harness.checkPoint("negative test");
    try {
      Double a;
      Byte b;
      a = new Double(0.1);
      b = new Byte((byte)1);
      compare(a,b,1);
      harness.fail("Exception should be thrown here.");
    } 
    catch (ClassCastException e) {
      // ok - exception was thrown
      harness.check(true);
    }
    try {
      Double a;
      String b;
      a = new Double(0.1);
      b = "foobar";
      compare(a,b,1);
      harness.fail("Exception should be thrown here.");
    } 
    catch (ClassCastException e) {
      // ok - exception was thrown
      harness.check(true);
    }
  }
}

