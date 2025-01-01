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
// Boston, MA 02111-1307, USA.

// Tags: JDK1.1

package gnu.testlet.java.util.Calendar;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.SimpleTimeZone;
import java.util.TimeZone;
import java.util.Locale;

public class set implements Testlet
{
  public void test (TestHarness harness)
  {
    testSimple(harness);
    test_DST(harness);
    test_DAY_OF_MONTH(harness);
    testUnsetFields(harness);
    testLenience(harness);
    testConflictingFields(harness);
    testNormalization(harness);
    testModSeconds(harness);
  }

  private void testSimple(TestHarness harness)
  {
    harness.checkPoint("Simple tests");
    Calendar c = Calendar.getInstance(Locale.FRANCE);
    c.setTimeZone(TimeZone.getTimeZone("GMT"));

    c.clear();
    c.set(Calendar.YEAR, 1980); 
    c.set(Calendar.MONTH, Calendar.JULY); 
    c.set(Calendar.DAY_OF_MONTH, 18); 
    c.set(Calendar.HOUR_OF_DAY, 22);
    c.set(Calendar.MINUTE, 13);
    c.set(Calendar.SECOND, 13);
    c.set(Calendar.MILLISECOND, 347);
    harness.check(c.getTime(), new Date(332806393347L));

    // Negative DAY_OF_WEEK_IN_MONTH is somewhat esoteric. Lets test.
    c.clear();
    c.set(Calendar.YEAR, 1980);
    c.set(Calendar.MONTH, Calendar.JULY);
    c.set(Calendar.DAY_OF_WEEK_IN_MONTH, -3);
    c.set(Calendar.DAY_OF_WEEK, Calendar.SUNDAY); 
    c.set(Calendar.HOUR_OF_DAY, 18);
    c.set(Calendar.MINUTE, 19);
    c.set(Calendar.SECOND, 12);
    c.set(Calendar.MILLISECOND, 519);
    harness.check(c.getTime(), new Date(332360352519L));
  }

  public void test_DST (TestHarness harness)
  {
    // Create a custom TimeZone with a daylight-time period.
    SimpleTimeZone stz = new SimpleTimeZone(60 * 60 * 1000, "MyZone",
		    Calendar.MARCH, -1, Calendar.SUNDAY, 2 * 60 * 60 * 1000,
		    Calendar.OCTOBER, -1, Calendar.SUNDAY, 2 * 60 * 60 * 1000);

    // Register the timezone as the default:
    TimeZone.setDefault(stz);
    
    Calendar cal = Calendar.getInstance(stz);
    Calendar cal2 = Calendar.getInstance(stz);

    cal.set(2004, Calendar.NOVEMBER, 4, 17, 30);

    harness.checkPoint ("Basic set/get");
    
    // Test field basics.
    harness.check (cal.get(Calendar.MINUTE), 30);
    harness.check (cal.get(Calendar.HOUR), 5);
    harness.check (cal.get(Calendar.MONTH), Calendar.NOVEMBER);
    harness.check (cal.get(Calendar.DAY_OF_WEEK), Calendar.THURSDAY);
    harness.check (cal.get(Calendar.AM_PM), Calendar.PM);
    harness.check (cal.get(Calendar.ZONE_OFFSET), 60 * 60 * 1000);
    harness.check (cal.get(Calendar.DST_OFFSET), 0);
    harness.check (cal.get(Calendar.WEEK_OF_MONTH), 1);

    // Now switch months.
    cal.set(Calendar.MONTH, Calendar.APRIL);
    harness.check (cal.get(Calendar.MONTH), Calendar.APRIL);
    harness.check (cal.get(Calendar.HOUR_OF_DAY), 17);

    harness.checkPoint ("moving calendar across DST boundary");
    
    // Check that hour is still correct after moving into a DST period.
    harness.check (cal.getTime().getHours(), 17);

    cal2.setTimeInMillis(cal.getTimeInMillis());
    harness.check (cal2.get(Calendar.HOUR_OF_DAY), 17);
      
    // Restore default timezone
    TimeZone.setDefault(null);
  }
  
  public void test_DAY_OF_MONTH(TestHarness harness)
  {
    harness.checkPoint("setting DAY_OF_MONTH etc shouldn't effect other fields");
    Calendar c = Calendar.getInstance(Locale.FRANCE);
    SimpleDateFormat df = new SimpleDateFormat("EEEEEEEEEEEEE, yyyy-MM-dd [DDD] HH:mm:ss.SSSS", Locale.US);
    c.set(2004, 9, 1, 12, 0, 0);
    c.set(Calendar.MILLISECOND, 0);
    
    String time = df.format(c.getTime());
    harness.check(time, "Friday, 2004-10-01 [275] 12:00:00.0000");
    
    c.set(Calendar.DAY_OF_MONTH, 31);
    time = df.format(c.getTime());
    harness.check(time, "Sunday, 2004-10-31 [305] 12:00:00.0000");

    c.set(Calendar.MONTH, Calendar.JANUARY);
    time = df.format(c.getTime());
    harness.check(time, "Saturday, 2004-01-31 [031] 12:00:00.0000");
  }

  private void testUnsetFields(TestHarness harness)
  {
    harness.checkPoint("setting only some fields");

    Calendar c = Calendar.getInstance(Locale.FRANCE);
    c.setTimeZone(TimeZone.getTimeZone("GMT"));

    c.clear();
    harness.check(c.getTime(), new Date(0)); // 1970-01-01T00:00Z

    c.clear();
    c.set(Calendar.YEAR, 1982);
    harness.check(c.getTime(), new Date(378691200000L)); // 1982-01-01T00:00Z

    c.clear();
    c.set(Calendar.DAY_OF_WEEK, Calendar.SUNDAY);
    harness.check(c.getTime(), new Date(259200000L)); // 1970-01-04T00:00Z

    c.clear();
    c.set(Calendar.DAY_OF_WEEK_IN_MONTH, 3);
    harness.check(c.getTime(), new Date(1555200000L)); // 1970-01-19T00:00Z

    c.clear();
    c.set(Calendar.WEEK_OF_YEAR, 2);
    harness.check(c.getTime(), new Date(345600000L)); // 1970-01-05T00:00Z

    c.clear();
    c.set(Calendar.YEAR, 1978);
    c.set(Calendar.MONTH, Calendar.AUGUST);
    harness.check(c.getTime(), new Date(270777600000L)); // 1978-08-01T00:00Z

    c.clear();
    c.set(Calendar.YEAR, 2004);
    c.set(Calendar.MONTH, Calendar.NOVEMBER);
    c.set(Calendar.DAY_OF_WEEK, Calendar.SATURDAY);
    harness.check(c.getTime(), new Date(1099699200000L)); // 2004-11-06T00:00Z
  }

  private void testLenience(TestHarness harness)
  {
    harness.checkPoint("test the setLenient() functionality");
    Calendar c = Calendar.getInstance(Locale.FRANCE);
    c.setLenient(false);
    c.set(Calendar.MONTH, 42);
    boolean b = false;
    try
      {
	c.get(Calendar.MONTH);
      }
    catch (IllegalArgumentException e)
      {
	b = true;
      }
    harness.check(b);
  }
  
  private void testConflictingFields(TestHarness harness)
  {
    harness.checkPoint("test setting conflicting values of different fields");
    Calendar c = Calendar.getInstance(Locale.FRANCE);
    c.setTimeZone(TimeZone.getTimeZone("GMT"));
    c.clear();

    c.set(Calendar.YEAR, 1997);
    // first setting day of year using one method
    c.set(Calendar.DAY_OF_YEAR, 55);
    // then setting another day with another method
    c.set(Calendar.DAY_OF_MONTH, 18);
    c.set(Calendar.MONTH, Calendar.MAY);
    harness.check(c.getTime(), new Date(863913600000L)); // 1997-05-18T08:00Z

    // the other way around
    c.clear();
    c.set(Calendar.HOUR_OF_DAY, 8);
    c.set(Calendar.YEAR, 1997);
    c.set(Calendar.HOUR_OF_DAY, 8);
    c.set(Calendar.DAY_OF_MONTH, 18);
    c.set(Calendar.MONTH, Calendar.MAY);
    c.set(Calendar.DAY_OF_YEAR, 55);
    harness.check(c.getTime(), new Date(856771200000L)); // 1997-02-24T08:00Z

    // trying three methods
    c.clear();
    c.set(Calendar.HOUR_OF_DAY, 8);
    c.set(Calendar.YEAR, 1997);
    c.set(Calendar.HOUR_OF_DAY, 8);
    c.set(Calendar.DAY_OF_MONTH, 18);
    c.set(Calendar.MONTH, Calendar.MAY);
    c.set(Calendar.DAY_OF_YEAR, 55);

    c.set(Calendar.MONTH, Calendar.AUGUST);
    c.set(Calendar.DAY_OF_WEEK_IN_MONTH, 3);
    c.set(Calendar.DAY_OF_WEEK, Calendar.TUESDAY);
    harness.check(c.getTime(), new Date(871977600000L)); // 1997-08-19T08:00Z

    // one interesting side effect of the algorithm for interpreting 
    // conflicting fields is that if not setting all the values in some
    // of the combinations described under "Calendar Fields Resolution" in
    // the spec then the value set in the incomplete combination will be
    // disregarded.
    c.clear();
    c.set(Calendar.DAY_OF_YEAR, 55);
    c.set(Calendar.MONTH, Calendar.AUGUST);
    harness.check(c.get(Calendar.MONTH), Calendar.FEBRUARY);

    c.clear();
    c.set(Calendar.HOUR_OF_DAY, 14);
    c.set(Calendar.HOUR, 8);
    harness.check(c.get(Calendar.HOUR), 2);
  }

  private void testNormalization(TestHarness harness)
  {
    harness.checkPoint("Normalization");
    Calendar c = Calendar.getInstance(Locale.FRANCE);
    c.setTimeZone(TimeZone.getTimeZone("GMT"));
    
    // negative HOUR_OF_DAY
    c.clear();
    c.set(Calendar.YEAR, 1980); 
    c.set(Calendar.MONTH, Calendar.JULY);
    c.set(Calendar.DAY_OF_MONTH, 18); 
    c.set(Calendar.HOUR_OF_DAY, -22);
    c.set(Calendar.MINUTE, 13);
    c.set(Calendar.SECOND, 13);
    harness.check(c.getTime(), new Date(332647993000L)); // 1980-07-17T02:13Z

    // HOUR == 12
    c.clear();
    c.set(Calendar.YEAR, 1980);
    c.set(Calendar.MONTH, Calendar.JULY);
    c.set(Calendar.DAY_OF_MONTH, 18);
    c.set(Calendar.HOUR, 12);
    c.set(Calendar.AM_PM, Calendar.AM);
    c.set(Calendar.MINUTE, 13);
    c.set(Calendar.SECOND, 13);
    harness.check(c.get(Calendar.HOUR), 0);
    harness.check(c.get(Calendar.AM_PM), Calendar.PM);

    // lets normalize ourselves into the leap day from a different year
    c.clear();
    c.set(Calendar.YEAR, 1997);
    c.set(Calendar.MONTH, Calendar.MARCH);
    c.set(Calendar.DAY_OF_MONTH, 1);
    c.set(Calendar.HOUR_OF_DAY, -366 * 24);
    harness.check(c.getTime(), new Date(825552000000L)); // 1996-02-29T00:00Z

   // XXX could have some fun here with leap seconds
  }

  private void testModSeconds(TestHarness harness)
  {
    harness.checkPoint("ModSeconds");
    Calendar c = Calendar.getInstance(Locale.FRANCE);
    c.setTimeZone(TimeZone.getTimeZone("GMT"));
    c.setLenient(true);
    c.set(Calendar.YEAR, 2005);
    c.set(Calendar.MONTH, 10);
    c.set(Calendar.DAY_OF_MONTH, 2);
    c.set(Calendar.HOUR, 2);
    c.set(Calendar.AM_PM, Calendar.AM);
    c.set(Calendar.MINUTE, 30);

    long t = c.getTimeInMillis() + 5500;

    c.setTimeInMillis(t);
    c.set(Calendar.SECOND, 0);
    c.set(Calendar.MILLISECOND, 0);
    c.getTime();
    harness.check(c.get(Calendar.YEAR), 2005);
    harness.check(c.get(Calendar.SECOND), 0);
  }
}
