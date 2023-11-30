// Tags: JDK1.1

// Copyright (c) 2003 Ito Kazumitsu

// This file is part of Mauve.

package gnu.testlet.java.util.Calendar;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.text.*;
import java.util.*;

public class ampm implements Testlet
{
  private SimpleDateFormat format;
  private TestHarness harness;

  public void test (TestHarness harness)
  {
    // AM/PM mark is locale-dependent.  We use Locale.US.
    format = new SimpleDateFormat("hh:mm a", Locale.US);
    String[] ampm = format.getDateFormatSymbols().getAmPmStrings();
    this.harness = harness;

    // According to the API document of java.util.Calendar,
    // midnight belongs to "am", and noon belongs to "pm".

    // NOTE: Calendar uses a 0-11 time, 
    // I.e. 0 AM (midnight), 1 AM, .. , 11 AM, midday = 0 PM, 1 PM .. 11 PM

    // Whereas the 'h' flag is 1-12 time
    // I.e. 12 AM (midnight), 1 AM, .. , 12 PM (midday), 1 PM, .. 11 PM

    checkTime("12:00 " + ampm[0], "12:00 " + ampm[0]);
    checkTime("12:10 " + ampm[0], "12:10 " + ampm[0]);
    checkTime(0, 0, Calendar.AM, "12:00 " + ampm[0]);
    checkTime("0:00 " + ampm[0], "12:00 " + ampm[0]);
    checkTime(0, 10, Calendar.AM, "12:10 " + ampm[0]);
    checkTime("0:10 " + ampm[0], "12:10 " + ampm[0]);

    checkTime("12:00 " + ampm[1], "12:00 " + ampm[1]);
    checkTime("12:10 " + ampm[1], "12:10 " + ampm[1]);
    checkTime(0, 0, Calendar.PM, "12:00 " + ampm[1]);
    checkTime("0:00 " + ampm[1], "12:00 " + ampm[1]);
    checkTime(0, 10, Calendar.PM, "12:10 " + ampm[1]);
    checkTime("0:10 " + ampm[1], "12:10 " + ampm[1]);
  }

  private void checkTime(int hh, int mm, int ampm, String expect)
  {

    Calendar calendar = Calendar.getInstance();
    calendar = Calendar.getInstance();
    calendar.set(Calendar.HOUR, hh);
    calendar.set(Calendar.MINUTE, mm);
    calendar.set(Calendar.AM_PM, ampm);
    harness.check (format.format (calendar.getTime()), expect);
  }

  private void checkTime(String input, String expect)
  {

    Calendar calendar = Calendar.getInstance();
    try
      {
        calendar.setTime (format.parse(input));
        harness.check (format.format(calendar.getTime()), expect);
      }
    catch (ParseException _)
      {
        harness.debug (_);
        harness.fail (input + " couldn't be parsed");
      }

  }

}
