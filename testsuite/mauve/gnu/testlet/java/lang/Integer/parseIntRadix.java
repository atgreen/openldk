// Test of Integer method parseInt(String, radix).

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

package gnu.testlet.java.lang.Integer;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

/**
 * Test the method Integer.parseInt(String, radix);
 */
public class parseIntRadix implements Testlet
{
  public void test (TestHarness harness)
  {
    int i;

    i = Integer.parseInt("0", 2);
    harness.check(i, 0);
    i = Integer.parseInt("0", 10);
    harness.check(i, 0);
    i = Integer.parseInt("0", 16);
    harness.check(i, 0);
    i = Integer.parseInt("0", 36);
    harness.check(i, 0);

    i = Integer.parseInt("10", 8);
    harness.check(i, 8);
    i = Integer.parseInt("10", 10);
    harness.check(i, 10);
    i = Integer.parseInt("10", 16);
    harness.check(i, 16);

    i = Integer.parseInt("z", 36);
    harness.check(i, 35);

    i = Integer.parseInt("-80", 16);
    harness.check(i, -128);

    i = Integer.parseInt("7f", 16);
    harness.check(i, 127);

    try
      {
	i = Integer.parseInt("10", Character.MIN_RADIX - 1);
	harness.fail("too small radix");
      }
    catch (NumberFormatException nfe)
      {
	harness.check(true);
      }

    try
      {
	i = Integer.parseInt("10", Character.MAX_RADIX + 1);
	harness.fail("too small radix");
      }
    catch (NumberFormatException nfe)
      {
	harness.check(true);
      }

    try
      {
	i = Integer.parseInt("-2147483649");
	harness.fail("-2147483649 is to small for a Integer");
      }
    catch (NumberFormatException nfe)
      {
	harness.check(true);
      }

    try
      {
	i = Integer.parseInt("2147483648");
	harness.fail("2147483648 is to big for a Integer");
      }
    catch (NumberFormatException nfe)
      {
	harness.check(true);
      }

    try
      {
        i = Integer.parseInt("abc");
	harness.fail("Illegal input (abc) must throw NumberFormatException");
      }
    catch (NumberFormatException nfe)
      {
	harness.check(true);
      }

    try
      {
        i = Integer.parseInt("-");
	harness.fail("Single '-' must throw NumberFormatException");
      }
    catch (NumberFormatException nfe)
      {
	harness.check(true);
      }

    try
    {
      i = Integer.parseInt("+");
	harness.fail("Single '+' must throw NumberFormatException");
    }
  catch (NumberFormatException nfe)
    {
	harness.check(true);
    }
  
    // In JDK1.7, '+' is considered a valid character.
    // it means that the following step should be divided
    // for pre JDK1.7 case and >= JDK1.7
    if (conformToJDK17()) {
      try
        {
          i = Integer.parseInt("+10", 10);
          harness.check(true);
          harness.check(i, 10);
        }
      catch (NumberFormatException nfe)
        {
          harness.fail("'+10' string is not parsed correctly as expected in JDK1.7");
        }
      }
    else { // pre JDK1.7 branch
      try
        {
          i = Integer.parseInt("+10", 10);
          harness.fail("'+10' must throw NumberFormatException");
        }
      catch (NumberFormatException nfe)
        {
          harness.check(true);
        }
      }

    try
      {
        i = Integer.parseInt(null, 10);
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
        i = Integer.parseInt("", 10);
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

  /**
    * Returns true if tested JRE conformns to JDK 1.7.
    * @author: Mark Wielaard
    */
  private static boolean conformToJDK17()
  {
    String[] javaVersion = System.getProperty("java.version").split("\\.");
    String vendorID = System.getProperty("java.vendor");
    // test of OpenJDK
    if ("Sun Microsystems Inc.".equals(vendorID))
      {
        return Integer.parseInt(javaVersion[1]) >= 7;
      }
    return true;
  }

}

