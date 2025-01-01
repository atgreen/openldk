// Test SimpleTimeZone.check14().

// Written by Jerry Quinn <jlquinn@optonline.net>

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

// Tags: JDK1.4

// Verify the constructors added in JDK 1.4

package gnu.testlet.java.util.SimpleTimeZone;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.util.*;

public class check14 implements Testlet
{
  public void test (TestHarness harness)
  {
    int rawOff = -18000000;	// 5 hours
    int dstOff = 3600000;	// 1 hour

    // Create a timezone for UTC-5 with daylight savings starting on
    // April 10 at 12 noon, ending September 10, 12 noon in daylight
    // savings, 1 hour shift.

    // All three should represent the same period
    SimpleTimeZone tzwall =
      new SimpleTimeZone(rawOff, "Z1",
			 4, 10, 0, 43200000, SimpleTimeZone.WALL_TIME,
			 9, 10, 0, 43200000, SimpleTimeZone.WALL_TIME,
			 dstOff);

    // Start time is same between WALL_TIME and STANDARD_TIME.  End
    // time is in STANDARD_TIME, not DST.  So ending at the same time
    // really means ending earlier in standard time.
    SimpleTimeZone tzstd =
      new SimpleTimeZone(rawOff, "Z2",
			 Calendar.MAY, 10, 0, 43200000, SimpleTimeZone.STANDARD_TIME,
			 Calendar.OCTOBER, 10, 0, 39600000, SimpleTimeZone.STANDARD_TIME,
			 dstOff);

    // Times are UTC, so later than 
    SimpleTimeZone tzutc =
      new SimpleTimeZone(rawOff, "Z3",
			 Calendar.MAY, 10, 0, 61200000, SimpleTimeZone.UTC_TIME,
			 Calendar.OCTOBER, 10, 0, 57600000, SimpleTimeZone.UTC_TIME,
			 dstOff);

    int wall;
    int std;
    int utc;

    // test 1/2 hour before dst
    wall = tzwall.getOffset(GregorianCalendar.AD, 2000, Calendar.MAY, 10, Calendar.WEDNESDAY, 41400000);
    std = tzstd.getOffset(GregorianCalendar.AD, 2000, Calendar.MAY, 10, Calendar.WEDNESDAY, 41400000);
    utc = tzutc.getOffset(GregorianCalendar.AD, 2000, Calendar.MAY, 10, Calendar.WEDNESDAY, 41400000);

    harness.check(wall, rawOff);  // check 1
    harness.check(std, rawOff);   // check 2
    harness.check(utc, rawOff);   // check 3
    
    // test 1/2 hour into dst
    wall = tzwall.getOffset(GregorianCalendar.AD, 2000, Calendar.MAY, 10, Calendar.WEDNESDAY, 45000000);
    std = tzstd.getOffset(GregorianCalendar.AD, 2000, Calendar.MAY, 10, Calendar.WEDNESDAY, 45000000);
    utc = tzutc.getOffset(GregorianCalendar.AD, 2000, Calendar.MAY, 10, Calendar.WEDNESDAY, 45000000);

    harness.check(wall, rawOff + dstOff);  // check 4
    harness.check(std, rawOff + dstOff);   // check 5
    harness.check(utc, rawOff + dstOff);   // check 6
    
    // test 1/2 hour before fall back to standard time
    wall = tzwall.getOffset(GregorianCalendar.AD, 2000, Calendar.OCTOBER, 10, Calendar.TUESDAY, 41400000 - dstOff);
    std = tzstd.getOffset(GregorianCalendar.AD, 2000, Calendar.OCTOBER, 10, Calendar.TUESDAY, 41400000 - dstOff);
    utc = tzutc.getOffset(GregorianCalendar.AD, 2000, Calendar.OCTOBER, 10, Calendar.TUESDAY, 41400000 - dstOff);

    harness.check(wall, rawOff + dstOff);  // check 7
    harness.check(std, rawOff + dstOff);   // check 8
    harness.check(utc, rawOff + dstOff);   // check 9
    
    // test 1/2 hour after fall back to standard time
    wall = tzwall.getOffset(GregorianCalendar.AD, 2000, Calendar.OCTOBER, 10, Calendar.TUESDAY, 45000000 - dstOff);
    std = tzstd.getOffset(GregorianCalendar.AD, 2000, Calendar.OCTOBER, 10, Calendar.TUESDAY, 45000000 - dstOff);
    utc = tzutc.getOffset(GregorianCalendar.AD, 2000, Calendar.OCTOBER, 10, Calendar.TUESDAY, 45000000 - dstOff);

    harness.check(wall, rawOff);           // check 10
    harness.check(std, rawOff);            // check 11
    harness.check(utc, rawOff);            // check 12

  }
}
