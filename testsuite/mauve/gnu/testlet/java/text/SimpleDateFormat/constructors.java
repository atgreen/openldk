// Tags: JDK1.2

// Copyright (C) 2005 David Gilbert <david.gilbert@object-refinery.com>

// This file is part of Mauve.

// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.

// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// B oston, MA 02111-1307, USA.  */

package gnu.testlet.java.text.SimpleDateFormat;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.text.DateFormatSymbols;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

/**
 * Some checks for the constructors in the SimpleDateFormat class.
 */
public class constructors implements Testlet
{

  /**
   * Runs the test using the specified harness. 
   * 
   * @param harness  the test harness (<code>null</code> not allowed).
   */
  public void test(TestHarness harness) 
  {
    testConstructor1(harness);
    testConstructor2(harness);
    testConstructor3(harness);
    testConstructor4(harness);
  }
  
  private void testConstructor1(TestHarness harness)
  {
    harness.checkPoint("SimpleDateFormat()"); 
    SimpleDateFormat f = new SimpleDateFormat();
    //String pattern = f.toPattern();
    
  }

  private void testConstructor2(TestHarness harness)
  {
    harness.checkPoint("SimpleDateFormat(String)"); 

    // check null argument
    try
    {
      /* SimpleDateFormat f = */ new SimpleDateFormat(null);   
      harness.check(false);
    }
    catch (NullPointerException e)
    {
      harness.check(true);   
    }
    
    // check invalid argument
    try
    {
      /* SimpleDateFormat f = */ new SimpleDateFormat("ZYXWVUT");   
      harness.check(false);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);   
    }
  }

  private void testConstructor3(TestHarness harness)
  {
    harness.checkPoint("SimpleDateFormat(String, DateFormatSymbols)"); 

    // bug 4099975 suggests that the DateFormatSymbols argument is 
    // cloned - check for this behaviour
    DateFormatSymbols s = new DateFormatSymbols(Locale.FRANCE);
    SimpleDateFormat f = new SimpleDateFormat("yyyy", s);
    harness.check(f.getDateFormatSymbols() != s);
    harness.check(f.getDateFormatSymbols().equals(s));

    // check null 'pattern' argument
    try
    {
      /* SimpleDateFormat f = */ new SimpleDateFormat(
        null, new DateFormatSymbols()
      );   
      harness.check(false);
    }
    catch (NullPointerException e)
    {
      harness.check(true);   
    }

    // check null 'formatData' argument
    try
    {
      /* SimpleDateFormat f = */ new SimpleDateFormat(
        "yyyy", (DateFormatSymbols) null
      );   
      harness.check(false);
    }
    catch (NullPointerException e)
    {
      harness.check(true);   
    }
    
    // check invalid argument
    try
    {
      /* SimpleDateFormat f = */ new SimpleDateFormat(
        "ZYXWVUT", new DateFormatSymbols()
      );   
      harness.check(false);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);   
    }
  }

  private void testConstructor4(TestHarness harness)
  {
    harness.checkPoint("SimpleDateFormat(String, Locale)"); 
    
    // check null 'pattern' argument
    try
    {
      /* SimpleDateFormat f = */ new SimpleDateFormat(null, Locale.UK);   
      harness.check(false);
    }
    catch (NullPointerException e)
    {
      harness.check(true);   
    }

    // check null 'locale' argument
    // the behaviour isn't specified (see bug 5061189) but here I'll 
    // assume NullPointerException
    try
    {
      /* SimpleDateFormat f = */ new SimpleDateFormat(
        "yyyy", (Locale) null
      );   
      harness.check(false);
    }
    catch (NullPointerException e)
    {
      harness.check(true);   
    }
    
    // check invalid argument
    try
    {
      /* SimpleDateFormat f = */ new SimpleDateFormat("ZYXWVUT", Locale.UK);   
      harness.check(false);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);   
    }
  }

}
