// Tags: JDK1.1

// Copyright (C) 2004 Stephen Crawley <crawley@dstc.edu.au>

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

package gnu.testlet.java.util.GregorianCalendar;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.util.*;

/**
 * Checks conversion of millisecond time values to Gregorian dates (and
 * back) across the range of time values.
 */
public class conversion implements Testlet
{
  private TestHarness harness;

  public void test (TestHarness harness)
  {
    this.harness = harness;
    testTimeZero();
    testMonotonic1();   
    testMonotonic2();
  }
  
  private void testTimeZero() 
  {
    GregorianCalendar cal = new GregorianCalendar();
    cal.setTimeZone(TimeZone.getTimeZone("UTC"));

    cal.setTimeInMillis(0L);
    harness.checkPoint("Testing setTimeInMillis(0L)");
    harness.check(cal.getTimeInMillis(), 0L);
    harness.check(cal.get(Calendar.ERA), GregorianCalendar.AD);
    harness.check(cal.get(Calendar.YEAR), 1970);
    harness.check(cal.get(Calendar.MONTH), Calendar.JANUARY);
    harness.check(cal.get(Calendar.DAY_OF_MONTH), 1);
    harness.check(cal.get(Calendar.HOUR_OF_DAY), 0);
    harness.check(cal.get(Calendar.MINUTE), 0);
    harness.check(cal.get(Calendar.SECOND), 0);
    harness.check(cal.get(Calendar.MILLISECOND), 0);
  }

  private void testMonotonic1() 
  {
    GregorianCalendar cal = new GregorianCalendar();
    cal.setTimeZone(TimeZone.getTimeZone("UTC"));

    // Check in range 0 <= t <= max long
    long[] times = new long[64];
    times[0] = 0L;
    for (int i = 1; i < 63; i++) {
      times[i] = 1L << i;
    }
    times[63] = Long.MAX_VALUE;
    
    cal.setTimeInMillis(times[0]);
    long[] prevFields = getCalFields(cal);
    for (int i = 1; i < times.length; i++) {
      cal.setTimeInMillis(times[i]);
      long[] fields = getCalFields(cal);
      harness.checkPoint("Testing setTimeInMillis(" + times[i] + ") i = " + i);
      harness.check(fields[0], times[i]);
      for (int j = 1; j < fields.length; j++) {
        if (fields[j] != prevFields[j]) {
          harness.check(fields[j] > prevFields[j]);
          if (fields[j] < prevFields[j]) {
            harness.debug("cal field " + j + " " + 
                          dumpCalFields(fields) + " < " + 
                          dumpCalFields(prevFields));
          }
          break;
        }
      }
      prevFields = fields;
    }
  }

  private void testMonotonic2() 
  {
    GregorianCalendar cal = new GregorianCalendar();
    cal.setTimeZone(TimeZone.getTimeZone("UTC"));

    // Check in range min long <= t < 0
    long[] times = new long[63];
    for (int i = 0; i < 63; i++) {
      times[62 - i] = -1L << i;
    }
    
    cal.setTimeInMillis(times[0]);
    long[] prevFields = getCalFields(cal);
    for (int i = 1; i < times.length; i++) {
      cal.setTimeInMillis(times[i]);
      long[] fields = getCalFields(cal);
      harness.checkPoint("Testing setTimeInMillis(" + times[i] + ") i = " + i);
      harness.check(fields[0], times[i]);
      if (fields[1] == prevFields[1]) {
        for (int j = 2; j < fields.length; j++) {
          if (fields[j] != prevFields[j]) {
            // In the BC era, years are >= 1 and go backwards.
            boolean ok = 
              ((fields[j] > prevFields[j]) == (j > 2 || fields[1] == 1));
            harness.check(ok);
            if (!ok) {
              harness.debug("cal field " + j + " " + 
                            dumpCalFields(fields) + " < " + 
                            dumpCalFields(prevFields));
            }
            break;
          }
        }
      }
      prevFields = fields;
    }
  }

  private long[] getCalFields(GregorianCalendar cal) {
    return new long[] {
      cal.getTimeInMillis(),
      cal.get(Calendar.ERA),
      cal.get(Calendar.YEAR),
      cal.get(Calendar.MONTH),
      cal.get(Calendar.DAY_OF_MONTH),
      cal.get(Calendar.HOUR_OF_DAY),
      cal.get(Calendar.MINUTE),
      cal.get(Calendar.SECOND),
      cal.get(Calendar.MILLISECOND)
    };
  }

  private String dumpCalFields(long[] fields) {
    StringBuffer sb = new StringBuffer();
    sb.append(fields[0]);
    for (int i = 1; i < fields.length; i++) {
      sb.append((i == 1) ? " {" : ", ");
      sb.append(fields[i]);
    }
    sb.append("}");
    return sb.toString();
  }
}
