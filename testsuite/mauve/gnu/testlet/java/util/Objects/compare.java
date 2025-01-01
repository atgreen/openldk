// Tags: JDK1.7

// Copyright (C) 2014 Andrew John Hughes (gnu_andrew@member.fsf.org)

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

package gnu.testlet.java.util.Objects;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.Comparator;
import java.util.Objects;

/**
 * Checks that {@link java.util.Objects#compare} performs
 * as specified.
 */
public class compare
  implements Testlet
{

  /**
   * Runs the test using the specified harness.
   *
   * @param harness the test harness.
   */
  public void test(TestHarness harness)
  {
    int result;

    result = Objects.compare(null, null, null);
    harness.debug("Objects.compare(null,null,null) = " + result);
    harness.check(result, 0, "compare(null,null,null) == 0");

    harness.checkPoint("Objects.compare(null,\"Potato\",c) throws NPE");
    try {
      result = Objects.compare(null, "Potato", new StringComparator());
      harness.debug("Objects.compare(null,\"Potato\",c) = " + result);
      harness.verbose("NullPointerException not propagated from Comparator");
      harness.check(false);
    } catch (NullPointerException e) {
      harness.check(true);
    }

    harness.checkPoint("Objects.compare(\"Potato\",null,c) throws NPE");
    try {
      result = Objects.compare("Potato", null, new StringComparator());
      harness.debug("Objects.compare(\"Potato\",null,c) = " + result);
      harness.verbose("NullPointerException not propagated from Comparator");
      harness.check(false);
    } catch (NullPointerException e) {
      harness.check(true);
    }

    result = Objects.compare("Potato", "Onion", new StringComparator());
    harness.debug("Objects.compare(\"Potato\", \"Onion\", c) = " + result);
    harness.check(result, 1, "\"Potato\" > \"Onion\"");

    result = Objects.compare("Onion", "Potato", new StringComparator());
    harness.debug("Objects.compare(\"Onion\", \"Potato\", c) = " + result);
    harness.check(result, -1, "\"Onion\" < \"Potato\"");

    result = Objects.compare("Potato", "Potato", new StringComparator());
    harness.debug("Objects.compare(\"Potato\", \"Potato\", c) = " + result);
    harness.check(result, 0, "\"Potato\" == \"Potato\"");
  }

  private static class StringComparator implements Comparator<String>
  {
    public int compare(String a, String b) { return a.compareTo(b); }
    public boolean equals(Object o) { return o == this; }
  }

}
