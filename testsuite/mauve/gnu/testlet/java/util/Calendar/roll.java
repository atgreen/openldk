// Test Calendar.roll().

// Copyright (c) 2001  Red Hat
// Written by Tom Tromey <tromey@redhat.com>

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
import java.text.*;
import java.util.*;

public class roll implements Testlet
{
  public void test (TestHarness harness)
  {
    java.util.TimeZone tz = java.util.TimeZone.getTimeZone ("GMT");
    DateFormat cdf = new SimpleDateFormat ("EEE, d MMM yyyy HH:mm:ss zzz",
					   Locale.US);
    cdf.setTimeZone (tz);

    // We use the US locale since we need a fixed one, and this one I
    // understand.
    Calendar k = Calendar.getInstance (tz, Locale.US);
    Date epoch = new Date (0);
    k.setTime (epoch);

    // Just double-check to make sure the tests themselves are ok.
    harness.check (cdf.format (epoch), "Thu, 1 Jan 1970 00:00:00 GMT");
    harness.check (k.getTime (), epoch);

    // No-op.
    k.roll (Calendar.YEAR, 0);
    harness.check (k.getTime (), epoch, "no-op add()");

    k.roll (Calendar.YEAR, 10);
    harness.check (k.get (Calendar.YEAR), 1980, "roll() year");
    harness.check (cdf.format (k.getTime ()),
		   "Tue, 1 Jan 1980 00:00:00 GMT");

    k.roll (Calendar.MONTH, -3);
    harness.check (k.get (Calendar.MONTH), Calendar.OCTOBER,
		   "roll() month");
    harness.check (k.get (Calendar.YEAR), 1980);
  }
}
