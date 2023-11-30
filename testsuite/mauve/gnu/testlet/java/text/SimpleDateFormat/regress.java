// Regression test for libgcj/Classpath SimpleDateFormat bugs

// Tags: JDK1.1

// Copyright (c) 1999, 2001, 2003, 2005  Free Software Foundation

// This file is part of Mauve.

package gnu.testlet.java.text.SimpleDateFormat;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.text.*;
import java.util.*;

public class regress implements Testlet
{
  // These must all be in the same format, with the timezone as the
  // characters after the final space, since that is what the code
  // expects.  They must also all represent the same time.
  public static String[] dates =
  {
    "Fri, 18 May 2001 12:18:06 CDT",
    "Fri, 18 May 2001 13:18:06 EDT",
    "Fri, 18 May 2001 12:18:06 EST",
    "Fri, 18 May 2001 17:18:06 GMT",
    "Fri, 18 May 2001 10:18:06 PDT"
  };

  public void test (TestHarness harness)
  {
    // We don't check the results but just that this works at all.  This
    // is a regression test for libgcj.
    harness.checkPoint ("parsing regression");
    DateFormat cdf = new SimpleDateFormat ("EEE, dd MMM yyyy HH:mm:ss zzzz");
    boolean ok = true;
    Date d = null;
    try
      {
	d = cdf.parse ("Fri, 18 May 2001 20:18:06 GMT");
      }
    catch (ParseException _)
      {
	ok = false;
      }
    harness.check (ok);

    Calendar k = Calendar.getInstance (TimeZone.getTimeZone ("GMT"));
    k.setTime (d);
    harness.check (k.get(Calendar.HOUR),        8, "check hour");
    harness.check (k.get(Calendar.HOUR_OF_DAY), 20, "check hour-of-day");

    cdf = new SimpleDateFormat ("EEE, dd MMM yyyy HH:mm:ss zzz");
    cdf.setTimeZone (TimeZone.getTimeZone ("GMT"));
    for (int i = 0; i < dates.length; ++i)
      {
	String tz = dates[i].substring (dates[i].lastIndexOf (' ') + 1,
					dates[i].length ());
	try
	  {
	    d = cdf.parse (dates[i]);
	    harness.check (cdf.format (d), "Fri, 18 May 2001 17:18:06 GMT",
			   tz);
	  }
	catch (ParseException _)
	  {
	    harness.debug ("At index " + _.getErrorOffset() + " " + _);
	    harness.check (false, tz);
	  }
      }

    cdf = new SimpleDateFormat("MM-dd-yyyy HH:mm:ss"); 
    try
      {
	d = cdf.parse ("03-22-2001 15:54:27");
	harness.check (cdf.format (d), "03-22-2001 15:54:27",
		       "local timezone");
      }
    catch (ParseException _)
      {
	harness.debug (_);
	harness.check (false, "local timezone");
      }

    DateFormat f = new SimpleDateFormat ("yyyy-MM-dd");
    GregorianCalendar g = new GregorianCalendar (1, 0, 1, 12, 0, 0);
    harness.check (f.format(g.getTime()), "0001-01-01",
		   "4 digit year");

    f = new SimpleDateFormat("''yyyy-MM-dd''");
    harness.check (f.format(g.getTime()), "'0001-01-01'",
		   "quoting 1");

    f = new SimpleDateFormat("'' '' '''FOO''' '' ''");
    harness.check (f.format(g.getTime()), "' ' 'FOO' ' '",
		   "quoting 2");

    long someTime = 1098968427000L; // 04-10-28 14:00:27 GMT
    SimpleDateFormat sdf = new SimpleDateFormat("yy-MM-dd HHmmss z");

    sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
    String str = sdf.format(new Date(someTime));

    sdf.setTimeZone(TimeZone.getTimeZone("CET"));
    try
      {
        harness.check(sdf.parse(str).getTime(), someTime,
                      "DST timezone");
      }
     catch (ParseException _)
      {
        harness.debug (_);
        harness.check (false, "DST timezone");
      }


     sdf = new SimpleDateFormat("yy-MM-dd HH:mm:ss Z");
     sdf.setTimeZone(TimeZone.getTimeZone("America/New_York"));
     someTime = 1098968427000L; // 04-10-28 14:00:27 GMT
     harness.check(sdf.format(new Date(someTime)), "04-10-28 09:00:27 -0400");

     harness.checkPoint("PR 28658");
     sdf = new SimpleDateFormat("EEE MMM dd HH:mm:ss yyyy", Locale.US);
     try
     {
    	 Date d1 = sdf.parse("Sun Nov 6 08:49:37 1994");
    	 Date d2 = sdf.parse("Sun Nov  6 08:49:37 1994");
    	 harness.check(d1, d2);
     }
     catch (ParseException _)
     {
    	 harness.debug(_);
    	 harness.debug("index: " + _.getErrorOffset());
    	 harness.check(false);
     }
  }
}
