// Tags: JDK1.1

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Mark Wielaard (mark@klomp.org)

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
 * Checks that the first day of the month is day one.
 */
public class first implements Testlet
{
  private TestHarness harness;

  public void test (TestHarness harness)
  {
    this.harness = harness;

    // Julian dates.
    testYears(1400);

    // Gregorian dates.
    testYears(2000);
  }

  private void testYears(int startYear)
  {
    for (int year = startYear; year <= startYear + 5; year++)
      for (int month = 0; month < 12; month++)
	{
	  GregorianCalendar cal = new GregorianCalendar(year, month, 1);
	  harness.check(cal.get(Calendar.DAY_OF_MONTH), 1,
			"day 1-" + month + "-" + year);
	}
  }
}
