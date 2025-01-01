// Tags: JDK1.1

// Copyright (C) 2004, 2005 Noa Resare <noa@resare.com>

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
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.text.SimpleDateFormat;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.text.*;
import java.util.*;

public class parse implements Testlet
{
  public void test(TestHarness harness)
  {
    SimpleDateFormat sdf = new SimpleDateFormat("MMMM yyyy", Locale.UK);
    sdf.setTimeZone(TimeZone.getTimeZone("UTC"));

    Map toTest = new HashMap();
    toTest.put("august 1978", new Date(270777600000L));
    toTest.put("August 1978", new Date(270777600000L));
    toTest.put("December 1978", new Date(281318400000L));
    doParse(harness, sdf, toTest);

    sdf.applyPattern("EEEE MMMM yyyy");
    toTest.clear();
    toTest.put("Saturday November 2004", new Date(1099699200000L));
    doParse(harness, sdf, toTest);

    sdf.applyPattern("yyyy-MM-dd HH:mm z");
    toTest.clear();
    toTest.put("2004-08-11 10:42 GMT", new Date(1092220920000L));
    toTest.put("2004-08-11 10:42 GMT+00:00", new Date(1092220920000L));
    toTest.put("2004-08-11 10:42 GMT-00:00", new Date(1092220920000L));
    toTest.put("2004-08-11 12:42 CEST", new Date(1092220920000L));
    toTest.put("2004-08-11 12:42 GMT+02:00", new Date(1092220920000L));
    toTest.put("2004-08-11 12:42 +0200", new Date(1092220920000L));
    doParse(harness, sdf, toTest);

    // Z should work exactly as z when parsing
    sdf.applyPattern("yyyy-MM-dd HH:mm Z");
    doParse(harness, sdf, toTest);

    // long and short names should both work.
    sdf = new SimpleDateFormat("EEE MMM", Locale.UK);
    sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
    toTest.clear();
    toTest.put("Sat Jun", new Date(13478400000L)); //saturday, june 6th, 1970, 02:00:00
    toTest.put("Saturday June", new Date(13478400000L));
    doParse(harness, sdf, toTest);
    sdf.applyPattern("EEEE MMMM");
    doParse(harness, sdf, toTest);

    /* Test case from bug #11583 */
    SimpleDateFormat sdf1 = new SimpleDateFormat("MMM dd, yyyy", Locale.UK);
    sdf1.setTimeZone(TimeZone.getTimeZone("UTC"));
    toTest = new HashMap();
    toTest.put("dec 31, 2004", new Date(1104451200000L));
    doParse(harness, sdf1, toTest);
    
    // test a case that is failing in statcvs and is the same as (I think) the
    // bug described in bug 13058
    harness.checkPoint("Bug 13058");
    SimpleDateFormat sdf2 = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss zzzzz", Locale.US);
    Date d = null;
    try
    {
      d = sdf2.parse("2004-07-18 17:42:25 +0000 GMT");        
    }
    catch (ParseException e)
    {
      // failure will be caught below  
    }
    harness.check(new Date(1090172545000L).equals(d));
    
    // test null arguments
    harness.checkPoint("Null arguments");
    SimpleDateFormat df = new SimpleDateFormat("dd-MMM-yyyy");
    boolean pass = false;
    try
    {
      df.parse(null, new ParsePosition(0));
    }
    catch (NullPointerException e)
    {
      pass = true;   
    }
    harness.check(pass);
    
    pass = false;
    try
    {
      df.parse("17-May-2005", null);   
    }
    catch (NullPointerException e)
    {
      pass = true;   
    }
    harness.check(pass);
  }

  /**
   * Test if the date strings in toTest equals to the Date values when parsed
   * with sdf.
   */
  private static void doParse(TestHarness h, SimpleDateFormat sdf, Map toTest)
  {
    h.checkPoint("parse pattern " + sdf.toPattern());
    Iterator cases = toTest.keySet().iterator();
    while (cases.hasNext())
      {
	String dateString = (String)cases.next();
        try
	  {
	    h.check(sdf.parse(dateString), toTest.get(dateString));
          }
        catch(Exception e)
	  {
	    h.check(false, e.getClass().getName() + ": ");
	    h.debug(e);
	  }
      }
  }
}
