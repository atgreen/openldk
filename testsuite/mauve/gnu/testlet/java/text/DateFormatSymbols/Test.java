/*************************************************************************
/* Test.java -- Test java.text.DateFormatSymbols
/*
/* Copyright (c) 1998, 2001 Free Software Foundation, Inc.
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

package gnu.testlet.java.text.DateFormatSymbols;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.text.*;
import java.util.Locale;
import java.util.MissingResourceException;

public class Test implements Testlet
{

private String[] my_eras =  { "XX", "YY" };
private String[] my_months = { "A", "B", "C", "D" };
private String[] my_short_months = { "a", "a", "b", "c" };
private String[] my_weekdays = { "S", "M", "T" };
private String[] my_short_weekdays = { "s", "m", "t" };
private String[] my_ampms = { "aa", "pp" };
private String[][] my_zonestrings = {{ "A", "B" }};
private String my_patternchars = "123456789012345678";

private static boolean
arrayEquals(Object[] o1, Object[] o2)
{
  if (o1 == null)
    {
      if (o2 != null)
        return(false);
    }
  else
    if (o2 == null)
      return(true);

  // We assume ordering is important.
  for (int i = 0; i < o1.length; i++)
    if (o1[i] instanceof Object[])
      {
        if (o2[i] instanceof Object[]) 
          {
            if (!arrayEquals((Object[])o1[i], (Object[])o2[i]))
              return(false);
          }
        else
          return(false);
      }
    else
      if (!o1[i].equals(o2[i]))
        return(false);

  return(true);
}

private static void
arrayDump(TestHarness harness, Object[] o, String desc)
{
  harness.debug("Dumping Object Array: " + desc);
  if (o == null)
    {
      harness.debug("null");
      return;
    }

  for (int i = 0; i < o.length; i++)
    if (o[i] instanceof Object[])
       arrayDump(harness, (Object[])o[i], desc + " element " + i);
    else
       harness.debug("  Element " + i + ": " + o[i]);
}

public void 
test(TestHarness harness)
{
  try
    {
      DateFormatSymbols dfs = new DateFormatSymbols(Locale.US);

      harness.debug("Dumping default symbol information");
      arrayDump(harness, dfs.getEras(), "eras"); 
      arrayDump(harness, dfs.getMonths(), "months"); 
      arrayDump(harness, dfs.getShortMonths(), "short months"); 
      arrayDump(harness, dfs.getWeekdays(), "weekdays"); 
      arrayDump(harness, dfs.getShortWeekdays(), "short weekdays"); 
      arrayDump(harness, dfs.getAmPmStrings(), "am/pm strings"); 
      arrayDump(harness, dfs.getZoneStrings(), "zone string array"); 
      harness.debug("local pattern chars: " + dfs.getLocalPatternChars()); 

      dfs.setEras(my_eras);
      harness.check(arrayEquals(dfs.getEras(), my_eras), "eras");

      dfs.setMonths(my_months);
      harness.check(arrayEquals(dfs.getMonths(), my_months), "months");

      dfs.setShortMonths(my_short_months);
      harness.check(arrayEquals(dfs.getShortMonths(), my_short_months), 
                    "short months");

      dfs.setWeekdays(my_weekdays);
      harness.check(arrayEquals(dfs.getWeekdays(), my_weekdays), "weekdays");

      dfs.setShortWeekdays(my_short_weekdays);
      harness.check(arrayEquals(dfs.getShortWeekdays(), my_short_weekdays), 
                    "short weekdays");

      dfs.setAmPmStrings(my_ampms);
      harness.check(arrayEquals(dfs.getAmPmStrings(), my_ampms), "am/pm");

      dfs.setLocalPatternChars(my_patternchars);
      harness.check(dfs.getLocalPatternChars(), my_patternchars, "patterns");
      
      /* Invalid Argument */
      boolean fail = false;
      try {
    	  dfs.setZoneStrings(my_zonestrings);
      } catch (IllegalArgumentException e) {
		fail = true;
      }
      harness.check(fail, true, "InvalidArgumentException is thrown.");
      
    }
  catch(MissingResourceException e)
    {
      harness.debug(e);
      harness.check(false);
    }
}

} // class Test

