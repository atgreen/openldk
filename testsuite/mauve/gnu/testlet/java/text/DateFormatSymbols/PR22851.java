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

package gnu.testlet.java.text.DateFormatSymbols;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.text.DateFormatSymbols;
import java.util.Locale;

/**
 * A check for PR22851.  This checks that the zone
 * strings are allocated to the correct elements in the
 * array.  The Javadoc for Java 1.6 now specifies this as:
 * </p>
 * <table>
 * <tr><td>zoneStrings[i][0]</td><td>time zone ID</td></tr>
 * <tr><td>zoneStrings[i][1]</td><td>long name of zone in standard time</td></tr>
 * <tr><td>zoneStrings[i][2]</td><td>short name of zone in standard time</td></tr>
 * <tr><td>zoneStrings[i][3]</td><td>long name of zone in daylight saving time</td></tr>
 * <tr><td>zoneStrings[i][4]</td><td>short name of zone in daylight saving time</td></tr>
 * </table>
 */
public class PR22851
  implements Testlet
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness the test harness.
   */
  public void test(TestHarness harness)
  {
    String[][] zstrings = DateFormatSymbols.getInstance(Locale.UK).getZoneStrings();
    boolean checked = false;
    for (int a = 0; a < zstrings.length; ++a)
      {
	harness.check(zstrings[a].length >= 5, zstrings[a][0] + " has less than 5 strings.");
	if (zstrings[a][0].equals("Europe/London"))
	  {
	    if (checked)
	      harness.fail("Europe/London appears twice.");
	    harness.check(zstrings[a][1], "Greenwich Mean Time");
	    harness.check(zstrings[a][2], "GMT");
	    harness.check(zstrings[a][3], "British Summer Time");
	    harness.check(zstrings[a][4], "BST");
	    checked = true;
	  }
      }
    if (!checked)
      harness.fail("Europe/London doesn't appear");
  }

}
