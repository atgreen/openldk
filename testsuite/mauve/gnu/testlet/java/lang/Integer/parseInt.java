/* Copyright (C) 2002 Free Software Foundation, Inc.
 * Written by Mark Wielaard <mark@klomp.org>
 *
 * This file is part of Mauve.
 *
 * Mauve is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * Mauve is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Mauve; see the file COPYING.  If not, write to
 * the Free Software Foundation, 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.  */

// Tags: JDK1.1

package gnu.testlet.java.lang.Integer;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class parseInt implements Testlet
{
  /**
    * Returns true if tested JRE conformns to JDK 1.7.
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

  public void test(TestHarness harness)
  {
    int i;

    i = Integer.parseInt("0");
    harness.check(i, 0);

    i = Integer.parseInt("1");
    harness.check(i, 1);

    i = Integer.parseInt("000");
    harness.check(i, 0);

    i = Integer.parseInt("007");
    harness.check(i, 7);

    i = Integer.parseInt("-0");
    harness.check(i, 0);

    i = Integer.parseInt("-1");
    harness.check(i, -1);

    i = Integer.parseInt("-2147483648");
    harness.check(i, Integer.MIN_VALUE);

    i = Integer.parseInt("2147483647");
    harness.check(i, Integer.MAX_VALUE);

    try
      {
	i = Integer.parseInt("-2147483649");
	harness.fail("-2147483649 is to small for an int");
      }
    catch (NumberFormatException nfe)
      {
	harness.check(true);
      }

    try
      {
	i = Integer.parseInt("2147483648");
	harness.fail("2147483648 is to big for an int");
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
          i = Integer.parseInt("+10");
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
          i = Integer.parseInt("+10");
          harness.fail("'+10' must throw NumberFormatException");
        }
      catch (NumberFormatException nfe)
        {
          harness.check(true);
        }
      }

    try
      {
        i = Integer.parseInt(null);
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
        i = Integer.parseInt("");
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
