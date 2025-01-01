// Tags: JDK1.2

// Copyright (C) 2008 Andrew John Hughes (gnu_andrew@member.fsf.org)

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
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.util.TimeZone;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.Locale;
import java.util.TimeZone;

/**
 * Checks that the correct strings are returned in the appropriate
 * slots for TimeZone.getDisplayName.  We use Europe/London for these
 * tests.
 */
public class GetDisplayName
  implements Testlet
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness the test harness.
   */
  public void test(TestHarness harness)
  {
    TimeZone zone = TimeZone.getTimeZone("Europe/London");
    harness.check(zone.getDisplayName(false, TimeZone.LONG, Locale.UK), "Greenwich Mean Time");
    harness.check(zone.getDisplayName(false, TimeZone.SHORT, Locale.UK), "GMT");
    harness.check(zone.getDisplayName(true, TimeZone.LONG, Locale.UK), "British Summer Time");
    harness.check(zone.getDisplayName(true, TimeZone.SHORT, Locale.UK), "BST");
  }

}
