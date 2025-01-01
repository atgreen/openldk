// Test Calendar.getActualMinimum().

// Copyright (c) 2004  Free Software Foundation.
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

// Tags: JDK1.2

package gnu.testlet.java.util.Calendar;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.text.*;
import java.util.*;

public class minmax implements Testlet
{
  public void test (TestHarness harness)
  {
    java.util.TimeZone tz = java.util.TimeZone.getTimeZone ("GMT");
    DateFormat cdf = new SimpleDateFormat ("EEE, d MMM yyyy HH:mm:ss zzz");
    cdf.setTimeZone (tz);

    // We use the US locale since we need a fixed one, and this one I
    // understand.
    Calendar k = Calendar.getInstance (tz, Locale.US);
    Date epoch = new Date (0);
    k.setTime (epoch);

    // Simple checks of getMinimum, getMaximum
    harness.check (k.getMinimum(Calendar.MONTH), Calendar.JANUARY, "min month");
    harness.check (k.getMaximum(Calendar.MONTH), Calendar.DECEMBER, "max month");
    harness.check (k.getMinimum(Calendar.DATE), 1, "min date");
    harness.check (k.getMaximum(Calendar.DATE), 31, "max date");
    harness.check (k.getMinimum(Calendar.DAY_OF_YEAR), 1, "min day of year");
    harness.check (k.getMaximum(Calendar.DAY_OF_YEAR), 366, "max day of year");

    // Check that getActualMaximum can generate a different value
    k.add (Calendar.MONTH, 3);
    harness.check (k.getActualMinimum(Calendar.DATE), 1, "actual min date");
    harness.check (k.getActualMaximum(Calendar.DATE), 30, "actual max date");
    harness.check (k.getActualMinimum(Calendar.DAY_OF_YEAR), 1, "actual min day of year");
    harness.check (k.getActualMaximum(Calendar.DAY_OF_YEAR), 365, "actual max day of year");
  }
}
