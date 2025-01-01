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

import java.util.Objects;

/**
 * Checks that {@link java.util.Objects#requireNonNull} performs
 * as specified.
 */
public class requireNonNull
  implements Testlet
{

  /**
   * Runs the test using the specified harness.
   *
   * @param harness the test harness.
   */
  public void test(TestHarness harness)
  {
    String result;
    String veg = "Potato";

    harness.checkPoint("Objects.requireNonNull(null) throws NPE");
    try {
      result = Objects.requireNonNull(null);
      harness.debug("Objects.requireNonNull(null) = " + result);
      harness.check(false);
    } catch (NullPointerException e) {
      harness.debug("Objects.requireNonNull(null) threw NPE");
      harness.check(true);
    }

    harness.checkPoint("Objects.requireNonNull(\"" + veg + "\") doesn't throw NPE");
    try {
      result = Objects.requireNonNull(veg);
      harness.debug("Objects.requireNonNull(\"" + veg + "\") = " + result);
      harness.check(true);
      harness.check(result, veg, "Objects.requireNonNull(\"" + veg + "\") returns \"" +
		    veg + "\"");
    } catch (NullPointerException e) {
      harness.debug("Objects.requireNonNull(" + veg + ") threw NPE");
      harness.check(false);
    }

    harness.checkPoint("Objects.requireNonNull(null,\"Oops\") throws NPE");
    try {
      result = Objects.requireNonNull(null,"Oops");
      harness.debug("Objects.requireNonNull(null,\"Oops\") = " + result);
      harness.check(false);
    } catch (NullPointerException e) {
      harness.debug("Objects.requireNonNull(null.\"Oops\") threw NPE");
      harness.check(true);
      harness.check(e.getMessage().contains("Oops"), "Specified message is used in NPE");
    }

    harness.checkPoint("Objects.requireNonNull(\"" + veg + "\", \"Oops\") doesn't throw NPE");
    try {
      result = Objects.requireNonNull(veg,"Oops");
      harness.debug("Objects.requireNonNull(\"" + veg + "\", \"Oops\") = " + result);
      harness.check(true);
      harness.check(result, veg, "Objects.requireNonNull(\"" + veg + "\", \"Oops\") returns \"" +
		    veg + "\"");
    } catch (NullPointerException e) {
      harness.debug("Objects.requireNonNull(" + veg + ") threw NPE");
      harness.check(false);
    }

  }

}
