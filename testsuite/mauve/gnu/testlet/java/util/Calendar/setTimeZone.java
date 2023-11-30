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

public class setTimeZone implements Testlet
{
  public void test (TestHarness harness)
  {
     Calendar cal = Calendar.getInstance();
     cal.setTimeZone(TimeZone.getTimeZone("GMT+0"));
     cal.setTime(new Date());
     int hour1 = cal.get(Calendar.HOUR_OF_DAY);
     cal.setTimeZone(TimeZone.getTimeZone("GMT-5"));
     int hour2 = cal.get(Calendar.HOUR_OF_DAY);
     int delta = (hour1 - hour2 + 24) % 24;
     harness.check(delta, 5);
  }
}
