// Tags: JDK1.4

// Copyright (C) 2004 David Gilbert <david.gilbert@object-refinery.com>

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
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.util.SimpleTimeZone;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.Calendar;
import java.util.SimpleTimeZone;

/**
 * Some checks for the constructors in the SimpleTimeZone class.
 */
public class constructors implements Testlet {

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
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
    harness.checkPoint("(int, String)");
    SimpleTimeZone z = new SimpleTimeZone(1234, "Z1");
    harness.check(z.getRawOffset(), 1234);
    harness.check(z.getID(), "Z1");
    
    // null id should throw exception
    try
    {
      SimpleTimeZone z2 = new SimpleTimeZone(0, null);
      harness.check(false);
    }
    catch (NullPointerException e) 
    {
      harness.check(true);
    }
  }
  
  private void testConstructor2(TestHarness harness) 
  {
    harness.checkPoint("(int, String, int, int, int, int, int, int, int, int)");
    SimpleTimeZone z = new SimpleTimeZone(1234, "Z1",
      Calendar.APRIL, 15, 0, 2*60*60*1000,
      Calendar.NOVEMBER, 22, 0, 2*60*60*1000);
    harness.check(z.getRawOffset(), 1234);
    harness.check(z.getID(), "Z1");
    harness.check(z.useDaylightTime());
    harness.check(z.getDSTSavings(), 60*60*1000);

    // null id should throw exception
    try
    {
      SimpleTimeZone z2 = new SimpleTimeZone(0, null, Calendar.APRIL, 15, 0, 2*60*60*1000,
        Calendar.NOVEMBER, 22, 0, 2*60*60*1000);
      harness.check(false);
    }
    catch (NullPointerException e) 
    {
      harness.check(true);
    }
    
    // check for invalid start month
    try
    {
      SimpleTimeZone z2 = new SimpleTimeZone(0, "Z", 12, 15, 0, 2*60*60*1000,
        Calendar.NOVEMBER, 22, 0, 2*60*60*1000);
      harness.check(false);
    }
    catch (IllegalArgumentException e) 
    {
      harness.check(true);
    }
    
    // check for invalid end month
    try
    {
      SimpleTimeZone z2 = new SimpleTimeZone(0, "Z", Calendar.APRIL, 15, 0, 2*60*60*1000,
        12, 22, 0, 2*60*60*1000);
      harness.check(false);
    }
    catch (IllegalArgumentException e) 
    {
      harness.check(true);
    }
    
    // check for invalid start day-of-month
    try
    {
      SimpleTimeZone z2 = new SimpleTimeZone(0, "Z", Calendar.APRIL, 33, 0, 2*60*60*1000,
        Calendar.NOVEMBER, 22, 0, 2*60*60*1000);
      harness.check(false);
    }
    catch (IllegalArgumentException e) 
    {
      harness.check(true);
    }
    
    // check for invalid end day-of-month
    try
    {
      SimpleTimeZone z2 = new SimpleTimeZone(0, "Z", Calendar.APRIL, 15, 0, 2*60*60*1000,
        Calendar.NOVEMBER, 33, 0, 2*60*60*1000);
      harness.check(false);
    }
    catch (IllegalArgumentException e) 
    {
      harness.check(true);
    }
    
    // check for invalid start n (for nth day-of-week)
    try
    {
      SimpleTimeZone z2 = new SimpleTimeZone(0, "Z", 
        Calendar.APRIL, 6, Calendar.MONDAY, 2*60*60*1000,
        Calendar.NOVEMBER, 22, 0, 2*60*60*1000);
      harness.check(false);
    }
    catch (IllegalArgumentException e) 
    {
      harness.check(true);
    }
    try
    {
      SimpleTimeZone z2 = new SimpleTimeZone(0, "Z", 
        Calendar.APRIL, -6, Calendar.MONDAY, 2*60*60*1000,
        Calendar.NOVEMBER, 22, 0, 2*60*60*1000);
      harness.check(false);
    }
    catch (IllegalArgumentException e) 
    {
      harness.check(true);
    }
    
    // check for invalid end n (for nth day-of-week)
    try
    {
      SimpleTimeZone z2 = new SimpleTimeZone(0, "Z", 
        Calendar.APRIL, 1, Calendar.MONDAY, 2*60*60*1000,
        Calendar.NOVEMBER, 6, Calendar.MONDAY, 2*60*60*1000);
      harness.check(false);
    }
    catch (IllegalArgumentException e) 
    {
      harness.check(true);
    }
    try
    {
      SimpleTimeZone z2 = new SimpleTimeZone(0, "Z", 
        Calendar.APRIL, 1, Calendar.MONDAY, 2*60*60*1000,
        Calendar.NOVEMBER, -6, Calendar.MONDAY, 2*60*60*1000);
      harness.check(false);
    }
    catch (IllegalArgumentException e) 
    {
      harness.check(true);
    }
    
  }
  
  private void testConstructor3(TestHarness harness) 
  {
    harness.checkPoint("(int, String, int, int, int, int, int, int, int, int, int)");
    SimpleTimeZone z = new SimpleTimeZone(1234, "Z1",
      Calendar.APRIL, 15, 0, 2*60*60*1000,
      Calendar.NOVEMBER, 22, 0, 2*60*60*1000, 123456);
    harness.check(z.getRawOffset(), 1234);
    harness.check(z.getID(), "Z1");
    harness.check(z.useDaylightTime());
    harness.check(z.getDSTSavings(), 123456);
    
    // null id should throw exception
    try
    {
      SimpleTimeZone z2 = new SimpleTimeZone(0, null, Calendar.APRIL, 15, 0, 2*60*60*1000,
        Calendar.NOVEMBER, 22, 0, 2*60*60*1000, 60*60*1000);
      harness.check(false);
    }
    catch (NullPointerException e) 
    {
      harness.check(true);
    }
    
    // check for invalid start month
    try
    {
      SimpleTimeZone z2 = new SimpleTimeZone(0, "Z", 12, 15, 0, 2*60*60*1000,
        Calendar.NOVEMBER, 22, 0, 2*60*60*1000, 60*60*1000);
      harness.check(false);
    }
    catch (IllegalArgumentException e) 
    {
      harness.check(true);
    }
    
    // check for invalid end month
    try
    {
      SimpleTimeZone z2 = new SimpleTimeZone(0, "Z", Calendar.APRIL, 15, 0, 2*60*60*1000,
        15, 22, 0, 2*60*60*1000, 60*60*1000);
      harness.check(false);
    }
    catch (IllegalArgumentException e) 
    {
      harness.check(true);
    }

    // check for invalid start day-of-month
    try
    {
      SimpleTimeZone z2 = new SimpleTimeZone(0, "Z", Calendar.APRIL, 33, 0, 2*60*60*1000,
        Calendar.NOVEMBER, 22, 0, 2*60*60*1000, 60*60*1000);
      harness.check(false);
    }
    catch (IllegalArgumentException e) 
    {
      harness.check(true);
    }

    // check for invalid end day-of-month
    try
    {
      SimpleTimeZone z2 = new SimpleTimeZone(0, "Z", Calendar.APRIL, 15, 0, 2*60*60*1000,
        Calendar.NOVEMBER, 33, 0, 2*60*60*1000, 60*60*1000);
      harness.check(false);
    }
    catch (IllegalArgumentException e) 
    {
      harness.check(true);
    }
  
    // check for invalid start n (for nth day of week)
    try
    {
      SimpleTimeZone z2 = new SimpleTimeZone(0, "Z", 
        Calendar.APRIL, 6, Calendar.MONDAY, 2*60*60*1000,
        Calendar.NOVEMBER, 22, 0, 2*60*60*1000, 60*60*1000);
      harness.check(false);
    }
    catch (IllegalArgumentException e) 
    {
      harness.check(true);
    }
    try
    {
      SimpleTimeZone z2 = new SimpleTimeZone(0, "Z", 
        Calendar.APRIL, -6, Calendar.MONDAY, 2*60*60*1000,
        Calendar.NOVEMBER, 22, 0, 2*60*60*1000, 60*60*1000);
      harness.check(false);
    }
    catch (IllegalArgumentException e) 
    {
      harness.check(true);
    }
    
    // check for invalid end n (for nth day of week)
    try
    {
      SimpleTimeZone z2 = new SimpleTimeZone(0, "Z", 
        Calendar.APRIL, 1, Calendar.MONDAY, 2*60*60*1000,
        Calendar.NOVEMBER, 6, Calendar.MONDAY, 2*60*60*1000, 60*60*1000);
      harness.check(false);
    }
    catch (IllegalArgumentException e) 
    {
      harness.check(true);
    }
    try
    {
      SimpleTimeZone z2 = new SimpleTimeZone(0, "Z", 
        Calendar.APRIL, -1, Calendar.MONDAY, 2*60*60*1000,
        Calendar.NOVEMBER, -6, Calendar.MONDAY, 2*60*60*1000, 60*60*1000);
      harness.check(false);
    }
    catch (IllegalArgumentException e) 
    {
      harness.check(true);
    }
  }
  
  private void testConstructor4(TestHarness harness) 
  {
    harness.checkPoint("(int, String, int, int, int, int, int, int, int, int, int, int, int)");
    SimpleTimeZone z = new SimpleTimeZone(1234, "Z1",
      Calendar.APRIL, 15, 0, 2*60*60*1000, SimpleTimeZone.UTC_TIME,
      Calendar.NOVEMBER, 22, 0, 2*60*60*1000, SimpleTimeZone.UTC_TIME,
      123456);
    harness.check(z.getRawOffset(), 1234);
    harness.check(z.getID(), "Z1");
    harness.check(z.useDaylightTime());
    harness.check(z.getDSTSavings(), 123456);

    // null id should throw exception
    try
    {
      SimpleTimeZone z2 = new SimpleTimeZone(0, null, 
        Calendar.APRIL, 15, 0, 2*60*60*1000, SimpleTimeZone.WALL_TIME,
        Calendar.NOVEMBER, 22, 0, 2*60*60*1000, SimpleTimeZone.WALL_TIME,
        60*60*1000);
      harness.check(false);
    }
    catch (NullPointerException e) 
    {
      harness.check(true);
    }
    
    // check for invalid start month
    try
    {
    SimpleTimeZone z2 = new SimpleTimeZone(1234, "Z1",
      12, 15, 0, 2*60*60*1000, SimpleTimeZone.UTC_TIME,
      Calendar.NOVEMBER, 22, 0, 2*60*60*1000, SimpleTimeZone.UTC_TIME,
      123456);
      harness.check(false);
    }
    catch (IllegalArgumentException e) 
    {
      harness.check(true);
    }
    
    // check for invalid end month
    try
    {
    SimpleTimeZone z2 = new SimpleTimeZone(1234, "Z1",
      Calendar.APRIL, 15, 0, 2*60*60*1000, SimpleTimeZone.UTC_TIME,
      12, 22, 0, 2*60*60*1000, SimpleTimeZone.UTC_TIME,
      123456);
      harness.check(false);
    }
    catch (IllegalArgumentException e) 
    {
      harness.check(true);
    }

    // check for invalid start day-of-month
    try
    {
    SimpleTimeZone z2 = new SimpleTimeZone(1234, "Z1",
      Calendar.APRIL, 33, 0, 2*60*60*1000, SimpleTimeZone.UTC_TIME,
      Calendar.NOVEMBER, 22, 0, 2*60*60*1000, SimpleTimeZone.UTC_TIME,
      123456);
      harness.check(false);
    }
    catch (IllegalArgumentException e) 
    {
      harness.check(true);
    }
    
    // check for invalid end day-of-month
    try
    {
    SimpleTimeZone z2 = new SimpleTimeZone(1234, "Z1",
      Calendar.APRIL, 15, 0, 2*60*60*1000, SimpleTimeZone.UTC_TIME,
      Calendar.NOVEMBER, 33, 0, 2*60*60*1000, SimpleTimeZone.UTC_TIME,
      123456);
      harness.check(false);
    }
    catch (IllegalArgumentException e) 
    {
      harness.check(true);
    }

    // check for invalid n (for nth day-of-week)
    try
    {
    SimpleTimeZone z2 = new SimpleTimeZone(1234, "Z1",
      Calendar.APRIL, 6, Calendar.MONDAY, 2*60*60*1000, SimpleTimeZone.UTC_TIME,
      Calendar.NOVEMBER, 22, 0, 2*60*60*1000, SimpleTimeZone.UTC_TIME,
      123456);
      harness.check(false);
    }
    catch (IllegalArgumentException e) 
    {
      harness.check(true);
    }
    
    try
    {
      SimpleTimeZone z2 = new SimpleTimeZone(1234, "Z1",
      Calendar.APRIL, -6, Calendar.MONDAY, 2*60*60*1000, SimpleTimeZone.UTC_TIME,
      Calendar.NOVEMBER, 22, 0, 2*60*60*1000, SimpleTimeZone.UTC_TIME,
      123456);
      harness.check(false);
    }
    catch (IllegalArgumentException e) 
    {
      harness.check(true);
    }

    // check for invalid n (for nth day-of-week)
    try
    {
    SimpleTimeZone z2 = new SimpleTimeZone(1234, "Z1",
      Calendar.APRIL, 1, Calendar.MONDAY, 2*60*60*1000, SimpleTimeZone.UTC_TIME,
      Calendar.NOVEMBER, 6, Calendar.MONDAY, 2*60*60*1000, SimpleTimeZone.UTC_TIME,
      123456);
      harness.check(false);
    }
    catch (IllegalArgumentException e) 
    {
      harness.check(true);
    }
    
    try
    {
      SimpleTimeZone z2 = new SimpleTimeZone(1234, "Z1",
      Calendar.APRIL, 1, Calendar.MONDAY, 2*60*60*1000, SimpleTimeZone.UTC_TIME,
      Calendar.NOVEMBER, -6, Calendar.MONDAY, 2*60*60*1000, SimpleTimeZone.UTC_TIME,
      123456);
      harness.check(false);
    }
    catch (IllegalArgumentException e) 
    {
      harness.check(true);
    }
  
  }

}
