/* getInstance.java -- some checks for the getInstance() methods in the
       Calendar class.
   Copyright (C) 2006 David Gilbert <david.gilbert@object-refinery.com>
This file is part of Mauve.

Mauve is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Mauve is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Mauve; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

*/

// Tags: JDK1.1

package gnu.testlet.java.util.Calendar;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.Calendar;
import java.util.Locale;

/**
 * Some checks for the getInstance() methods in the Calendar class.
 */
public class getInstance implements Testlet
{
  public void test(TestHarness harness) 
  {
    testMethod1(harness);  
    testMethod2(harness);  
    testMethod3(harness);  
    testMethod4(harness);  
  }
  
  public void testMethod1(TestHarness harness) 
  {
    harness.checkPoint("()");
    Calendar c = Calendar.getInstance();
    harness.check(c.getTimeZone(), java.util.TimeZone.getDefault());
    
    // check that the method returns a new instance each time
    Calendar c2 = Calendar.getInstance();
    harness.check(c != c2);
  }
  
  public void testMethod2(TestHarness harness) 
  {
    harness.checkPoint("(TimeZone)");
    Calendar c = Calendar.getInstance(java.util.TimeZone.getTimeZone("GMT"));
    harness.check(c.getTimeZone(), java.util.TimeZone.getTimeZone("GMT"));
    
    // check that the method returns a new instance each time
    Calendar c2 = Calendar.getInstance(java.util.TimeZone.getTimeZone("GMT"));
    harness.check(c != c2);

    // try null
    boolean pass = false;
    try 
    {
      /* Calendar c = */ Calendar.getInstance((java.util.TimeZone) null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
  }

  public void testMethod3(TestHarness harness) 
  {
    harness.checkPoint("(Locale)");       
    Calendar c = Calendar.getInstance(Locale.UK);
    Calendar c2 = Calendar.getInstance(Locale.UK);
    harness.check(c != c2);
        
    // try null
    boolean pass = false;
    try 
    {
      /* Calendar c = */ Calendar.getInstance((Locale) null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
  }

  public void testMethod4(TestHarness harness) 
  {
    harness.checkPoint("(TimeZone, Locale)");   
    Calendar c = Calendar.getInstance(java.util.TimeZone.getTimeZone("GMT"), 
            Locale.UK);
    Calendar c2 = Calendar.getInstance(java.util.TimeZone.getTimeZone("GMT"), 
            Locale.UK);
    harness.check(c != c2);

    // try null TimeZone
    boolean pass = false;
    try
    {
      /* Calendar c = */ Calendar.getInstance((java.util.TimeZone) null, 
            Locale.getDefault());
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // try null Locale
    pass = false;
    try
    {
      /* Calendar c = */ Calendar.getInstance(java.util.TimeZone.getDefault(), 
            (Locale) null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
    
  }
  
}
