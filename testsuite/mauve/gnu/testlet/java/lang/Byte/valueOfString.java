// Test of Byte method valueOf(String).

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
 * Test the method Byte.valueOf(String).
 */
public class valueOfString implements Testlet
{
  public void test (TestHarness harness)
  {
    harness.check(new Byte((byte)0).equals(Byte.valueOf("0")));
    harness.check(new Byte((byte)-1).equals(Byte.valueOf("-1")));
    harness.check(new Byte((byte)1).equals(Byte.valueOf("1")));
    harness.check(new Byte((byte)127).equals(Byte.valueOf("127")));
    harness.check(new Byte((byte)-128).equals(Byte.valueOf("-128")));

    try
      {
	Byte.valueOf("10", Character.MIN_RADIX - 1);
	harness.fail("too small radix");
      }
    catch (NumberFormatException nfe)
      {
	harness.check(true);
      }

    try
      {
	Byte.valueOf("10", Character.MAX_RADIX + 1);
	harness.fail("too small radix");
      }
    catch (NumberFormatException nfe)
      {
	harness.check(true);
      }

    try
      {
	Byte.valueOf("-129");
	harness.fail("-129 is to small for a byte");
      }
    catch (NumberFormatException nfe)
      {
	harness.check(true);
      }

    try
      {
	Byte.valueOf("128");
	harness.fail("128 is to big for a byte");
      }
    catch (NumberFormatException nfe)
      {
	harness.check(true);
      }

    try
      {
        Byte.valueOf("abc");
	harness.fail("Illegal input (abc) must throw NumberFormatException");
      }
    catch (NumberFormatException nfe)
      {
	harness.check(true);
      }

    try
      {
        Byte.valueOf("-");
	harness.fail("Single '-' must throw NumberFormatException");
      }
    catch (NumberFormatException nfe)
      {
	harness.check(true);
      }

    try
    {
        Byte.valueOf("+");
	harness.fail("Single '+' must throw NumberFormatException");
    }
  catch (NumberFormatException nfe)
    {
	harness.check(true);
    }
  
    try
      {
        Byte.valueOf(null, 10);
	harness.fail("null input must throw NumberFormatException");
      }
    catch (NullPointerException npe)
      {
	harness.fail("null input must throw NumberFormatException, not NullPointerException");
      }
    catch (NumberFormatException nfe)
      {
	harness.check(true);
      }
    
    try
      {
        Byte.valueOf("", 10);
	harness.fail("empty input must throw NumberFormatException");
      }
    catch (IndexOutOfBoundsException ioobe)
      {
	harness.fail("empty input must throw NumberFormatException, not IndexOutOfBoundsException");
      }
    catch (NumberFormatException nfe)
      {
	harness.check(true);
      }
    }
}

