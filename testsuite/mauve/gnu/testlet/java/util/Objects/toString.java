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
 * Checks that {@link java.util.Objects#toString} performs
 * as specified.
 */
public class toString
  implements Testlet
{

  /**
   * Runs the test using the specified harness.
   *
   * @param harness the test harness.
   */
  public void test(TestHarness harness)
  {
    String result, compResult;
    String veg = "Potato";

    result = Objects.toString(null);
    harness.debug("Objects.toString(null) = " + result);
    harness.check(result, "null", "Objects.toString(null) == \"null\"");

    result = Objects.toString(veg);
    compResult = veg.toString();
    harness.debug("Objects.toString(\"" + veg + "\") = " + result +
		  ", \"" + veg + "\".toString() = " + compResult);
    harness.check(result, compResult, "Objects.toString(\"" + veg +
		  "\") == \"" + veg + "\".toString()");

    result = Objects.toString(null,"Apricot");
    harness.debug("Objects.toString(null,\"Apricot\") = " + result);
    harness.check(result, "Apricot", "Objects.toString(null,\"Apricot\") == \"Apricot\"");

    result = Objects.toString(veg,"Apricot");
    harness.debug("Objects.toString(\"" + veg + "\",\"Apricot\") = " + result +
		  ", \"" + veg + "\".toString() = " + compResult);
    harness.check(result, compResult, "Objects.toString(\"" + veg + "\",\"Apricot\") == \"" +
		  veg + "\".toString()");
  }
}
