/*************************************************************************
/* Test.java -- Test java.text.SimpleDateFormat
/*
/* Copyright (c) 1998, 1999, 2001, 2003 Free Software Foundation, Inc.
/* Written by Aaron M. Renn (arenn@urbanophile.com)
/*
/* This program is free software; you can redistribute it and/or modify
/* it under the terms of the GNU General Public License as published 
/* by the Free Software Foundation, either version 2 of the License, or
/* (at your option) any later version.
/*
/* This program is distributed in the hope that it will be useful, but
/* WITHOUT ANY WARRANTY; without even the implied warranty of
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
/* GNU General Public License for more details.
/*
/* You should have received a copy of the GNU General Public License
/* along with this program; if not, write to the Free Software Foundation
/* Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307 USA
/*************************************************************************/

// Tags: JDK1.1

package gnu.testlet.java.text.SimpleDateFormat;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.text.*;
import java.util.*;

public class Test implements Testlet
{

public void 
test(TestHarness harness)
{
  String pattern_chars = "GyMdhHmsSEDFwWakKz";
  String pattern = "EEEE, MMMM d, yyyy h:mm:ss 'o''clock' a";

  DateFormatSymbols dfs = new DateFormatSymbols(Locale.US);
  SimpleDateFormat sdf = new SimpleDateFormat(pattern, dfs);
  harness.check(sdf.getDateFormatSymbols(), dfs, "getDateFormatSymbols() init");

  String[] ampms = { "am ", "pm " };
  dfs.setAmPmStrings(ampms);
  sdf.setDateFormatSymbols(dfs);
  harness.check(sdf.getDateFormatSymbols(), dfs, "set/getDateFormatSymbols()");
  
  harness.check(sdf.toPattern(), pattern, "toPattern init");
  String new_pattern = "EMdyH";
  sdf.applyPattern(new_pattern);
  harness.check(sdf.toPattern(), new_pattern, "apply/toPattern()");
  sdf.applyPattern(pattern);

  harness.check(sdf.equals(new SimpleDateFormat(pattern, dfs)), "equals()");
  harness.check(sdf.clone().equals(sdf) == true, "clone()");

  sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
  Date d = new Date(0);
  String formatted_date = sdf.format(d);
  harness.debug(formatted_date);
  harness.check(formatted_date.equals(
     "Thursday, January 1, 1970 12:00:00 o'clock am "), "format()");

  sdf.setLenient(false);
  try
    {
      harness.check(sdf.parse(formatted_date), d, "parse() strict");
    }
  catch(Throwable e)
    {
      harness.debug(e);
      harness.check(false, "parse() strict");
    }

  sdf.setTimeZone(TimeZone.getDefault());
  harness.debug(sdf.format(new Date(System.currentTimeMillis())));

  // Now do some lenient parsing tests.  These might not all work.
  dfs = new DateFormatSymbols(Locale.US);
  sdf = new SimpleDateFormat(pattern, dfs);

  sdf.setLenient(true);

  String[] date_strs = { 
    "Tue Feb 23 20:15:34 CST 1999",
    "10/31/69",
    "1999/02/23",
    "6.9.98 12:43pm",
    "Monday, February 22, 1999 10:24:43",
    "Wed Feb 24 19:35:02 1999 and a bunch more text",
    "Wed, 24 Feb 1999 05:12:21 GMT"
  };
   
  harness.debug("The following tests are informational only");
  for (int i = 0; i < date_strs.length; i++)
    {
      d = null;
      try
        {
          d = sdf.parse(date_strs[i]);
        }
      catch(Throwable e) { ; }
      if (d == null)
        harness.debug("Couldn't parse: " + date_strs[i]);
      else
        harness.debug("Parsed: " + date_strs[i] + " as: " + d);
    }

  sdf = new SimpleDateFormat("MM'/'dd'/'yyyy' 'H':'m':'s'.'SSS");
  boolean ok = true;
  try
    {
      d = sdf.parse("05/24/2002 14:30:53.700");
      // For the time being only check to make sure something
      // happened.
      ok = d != null;
    }
  catch (Exception _)
    {
      ok = false;
    }
  harness.check(ok, "format includes '.'");
}

} // class Test

