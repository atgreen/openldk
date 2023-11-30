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

import java.util.Arrays;
import java.util.Objects;

/**
 * Checks that {@link java.util.Objects#hash} performs
 * as specified.
 */
public class hash
  implements Testlet
{

  /**
   * Runs the test using the specified harness.
   *
   * @param harness the test harness.
   */
  public void test(TestHarness harness)
  {
    int result, compResult;
    String[] veg1 = {"Potato","Onion","Carrot"};

    result = Objects.hash((Object[]) null);
    harness.debug("Objects.hash(null) = " + result);
    harness.check(result, 0, "Objects.hash(null) == 0");

    harness.debug("veg1(" + veg1 + ") = " + Arrays.toString(veg1));

    result = Objects.hash(veg1[0], veg1[1], veg1[2]);
    compResult = Arrays.hashCode(veg1);
    harness.debug("Objects.hash(" + veg1[0] + "," + veg1[1] + "," + veg1[2] +
		  ") = " + result + ", Arrays.hashCode(" + veg1 + ") = " +
		  compResult);
    harness.check(result, compResult,
		  "Object.hash(veg1[0],veg1[1],veg1[2]) == Arrays.hashCode(veg1)");

    compResult = Arrays.asList(veg1).hashCode();
    harness.debug("Objects.hash(" + veg1[0] + "," + veg1[1] + "," + veg1[2] +
		       ") = " + result +
		  ", Arrays.asList(" + veg1 + ").hashCode = " + compResult);
    harness.check(result, compResult,
		  "Object.hash(veg1[0],veg1[1],veg1[2]) == Arrays.asList(veg1).hashCode()");
  }
}
