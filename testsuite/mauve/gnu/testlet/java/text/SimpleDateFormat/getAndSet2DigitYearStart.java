// Tags: JDK1.2

// Copyright (c) 1999, 2001  Free Software Foundation

// This file is part of Mauve.

package gnu.testlet.java.text.SimpleDateFormat;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.text.*;
import java.util.*;

public class getAndSet2DigitYearStart implements Testlet
{
  public void test (TestHarness harness)
    {
      String pattern = "EEEE, MMMM d, yyyy h:mm:ss 'o''clock' a";
      
      DateFormatSymbols dfs = new DateFormatSymbols(Locale.US);
      SimpleDateFormat sdf = new SimpleDateFormat(pattern, dfs);

      // I removed this test as it relied on the year never changing.
      // -tromey
      // This unusual value seems to be what the JDK outputs.
      // harness.check(sdf.get2DigitYearStart(), new Date(-1608614014805L), 
      // "get2DigitYearStart() initial");
      Date d = new Date(System.currentTimeMillis());
      sdf.set2DigitYearStart(d);
      harness.check(sdf.get2DigitYearStart(), d, "set/get2DigitYearStart()");
    }
}
