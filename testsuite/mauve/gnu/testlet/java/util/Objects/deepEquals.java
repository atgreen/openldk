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
 * Checks that {@link java.util.Objects#deepEquals} performs
 * as specified.
 */
public class deepEquals
  implements Testlet
{

  /**
   * Runs the test using the specified harness.
   *
   * @param harness the test harness.
   */
  public void test(TestHarness harness)
  {
    boolean bResult;
    String[] veg1 = {"Potato","Onion","Carrot"};
    String[] veg2 = {"Potato","Onion","Carrot"};
    boolean[] b1 = {true, true, false};
    boolean[] b2 = {true, true, false};
    byte[] by1 = {5, 42, -126};
    byte[] by2 = {5, 42, -126};
    char[] c1 = {'a', 'z', 'A', 'Z'};
    char[] c2 = {'a', 'z', 'A', 'Z'};
    double[] d1 = {0.5d, 3.14d, 6.5d};
    double[] d2 = {0.5d, 3.14d, 6.5d};
    float[] f1 = {0.5f, 3.14f, 6.5f};
    float[] f2 = {0.5f, 3.14f, 6.5f};
    int[] i1 = {5, 4096, 42};
    int[] i2 = {5, 4096, 42};
    long[] l1 = {5L, 4096L, 42L};
    long[] l2 = {5L, 4096L, 42L};
    Object[] nested1 = {"Field", veg1, veg2};
    Object[] nested2 = {"Field", veg1, veg2};
    short[] s1 = {5, -2048, 42};
    short[] s2 = {5, -2048, 42};

    bResult = Objects.deepEquals(null,null);
    harness.debug("Objects.deepEquals(null,null) = " + bResult);
    harness.check(bResult, true, "deepEquals(null,null) == true");

    bResult = Objects.deepEquals(null,"Potato");
    harness.debug("Objects.deepEquals(null,\"Potato\") = " + bResult);
    harness.check(bResult, false, "deepEquals(null,\"Potato\") == false");

    bResult = Objects.deepEquals("Potato",null);
    harness.debug("Objects.deepEquals(\"Potato\",null) = " + bResult);
    harness.check(bResult, false, "deepEquals(\"Potato\",null) == false");

    bResult = Objects.deepEquals("Potato","Potato");
    harness.debug("Objects.deepEquals(\"Potato\",\"Potato\") = " + bResult);
    harness.check(bResult, true, "deepEquals(\"Potato\",\"Potato\") == true");

    harness.debug("veg1(" + veg1 + ") = " + Arrays.toString(veg1));
    harness.debug("veg2(" + veg2 + ") = " + Arrays.toString(veg2));

    bResult = Objects.deepEquals(veg1, veg1);
    harness.debug("Objects.deepEquals(" + veg1 + "," + veg1 + ") = " + bResult);
    harness.check(bResult, true, "String array ==; deepEquals(" + veg1 + "," + veg1 + ") == true");

    bResult = Objects.deepEquals(veg1, veg2);
    harness.debug("Objects.deepEquals(" + veg1 + "," + veg2 + ") = " + bResult);
    harness.check(bResult, true, "String array !=; deepEquals(" + veg1 + "," + veg2 + ") == true");

    harness.debug("b1(" + b1 + ") = " + Arrays.toString(b1));
    harness.debug("b2(" + b2 + ") = " + Arrays.toString(b2));

    bResult = Objects.deepEquals(b1, b1);
    harness.debug("Objects.deepEquals(" + b1 + "," + b1 + ") = " + bResult);
    harness.check(bResult, true, "boolean array ==; deepEquals(" + b1 + "," + b1 + ") == true");

    bResult = Objects.deepEquals(b1, b2);
    harness.debug("Objects.deepEquals(" + b1 + "," + b2 + ") = " + bResult);
    harness.check(bResult, true, "boolean array !=; deepEquals(" + b1 + "," + b2 + ") == true");

    harness.debug("by1(" + by1 + ") = " + Arrays.toString(by1));
    harness.debug("by2(" + by2 + ") = " + Arrays.toString(by2));

    bResult = Objects.deepEquals(by1, by1);
    harness.debug("Objects.deepEquals(" + by1 + "," + by1 + ") = " + bResult);
    harness.check(bResult, true, "byte array ==; deepEquals(" + by1 + "," + by1 + ") == true");

    bResult = Objects.deepEquals(by1, by2);
    harness.debug("Objects.deepEquals(" + by1 + "," + by2 + ") = " + bResult);
    harness.check(bResult, true, "byte array !=; deepEquals(" + by1 + "," + by2 + ") == true");

    harness.debug("c1(" + c1 + ") = " + Arrays.toString(c1));
    harness.debug("c2(" + c2 + ") = " + Arrays.toString(c2));

    bResult = Objects.deepEquals(c1, c1);
    harness.debug("Objects.deepEquals(" + c1 + "," + c1 + ") = " + bResult);
    harness.check(bResult, true, "char array ==; deepEquals(" + c1 + "," + c1 + ") == true");

    bResult = Objects.deepEquals(c1, c2);
    harness.debug("Objects.deepEquals(" + c1 + "," + c2 + ") = " + bResult);
    harness.check(bResult, true, "char array !=; deepEquals(" + c1 + "," + c2 + ") == true");

    harness.debug("d1(" + d1 + ") = " + Arrays.toString(d1));
    harness.debug("d2(" + d2 + ") = " + Arrays.toString(d2));

    bResult = Objects.deepEquals(d1, d1);
    harness.debug("Objects.deepEquals(" + d1 + "," + d1 + ") = " + bResult);
    harness.check(bResult, true, "double array ==; deepEquals(" + d1 + "," + d1 + ") == true");

    bResult = Objects.deepEquals(d1, d2);
    harness.debug("Objects.deepEquals(" + d1 + "," + d2 + ") = " + bResult);
    harness.check(bResult, true, "double array !=; deepEquals(" + d1 + "," + d2 + ") == true");

    harness.debug("f1(" + f1 + ") = " + Arrays.toString(f1));
    harness.debug("f2(" + f2 + ") = " + Arrays.toString(f2));

    bResult = Objects.deepEquals(f1, f1);
    harness.debug("Objects.deepEquals(" + f1 + "," + f1 + ") = " + bResult);
    harness.check(bResult, true, "float array ==; deepEquals(" + f1 + "," + f1 + ") == true");

    bResult = Objects.deepEquals(f1, f2);
    harness.debug("Objects.deepEquals(" + f1 + "," + f2 + ") = " + bResult);
    harness.check(bResult, true, "float array !=; deepEquals(" + f1 + "," + f2 + ") == true");

    harness.debug("i1(" + i1 + ") = " + Arrays.toString(i1));
    harness.debug("i2(" + i2 + ") = " + Arrays.toString(i2));

    bResult = Objects.deepEquals(i1, i1);
    harness.debug("Objects.deepEquals(" + i1 + "," + i1 + ") = " + bResult);
    harness.check(bResult, true, "int array ==; deepEquals(" + i1 + "," + i1 + ") == true");

    bResult = Objects.deepEquals(i1, i2);
    harness.debug("Objects.deepEquals(" + i1 + "," + i2 + ") = " + bResult);
    harness.check(bResult, true, "int array !=; deepEquals(" + i1 + "," + i2 + ") == true");

    harness.debug("l1(" + l1 + ") = " + Arrays.toString(l1));
    harness.debug("l2(" + l2 + ") = " + Arrays.toString(l2));

    bResult = Objects.deepEquals(l1, l1);
    harness.debug("Objects.deepEquals(" + l1 + "," + l1 + ") = " + bResult);
    harness.check(bResult, true, "long array ==; deepEquals(" + l1 + "," + l1 + ") == true");

    bResult = Objects.deepEquals(l1, l2);
    harness.debug("Objects.deepEquals(" + l1 + "," + l2 + ") = " + bResult);
    harness.check(bResult, true, "long array !=; deepEquals(" + l1 + "," + l2 + ") == true");

    harness.debug("s1(" + s1 + ") = " + Arrays.toString(s1));
    harness.debug("s2(" + s2 + ") = " + Arrays.toString(s2));

    bResult = Objects.deepEquals(s1, s1);
    harness.debug("Objects.deepEquals(" + s1 + "," + s1 + ") = " + bResult);
    harness.check(bResult, true, "short array ==; deepEquals(" + s1 + "," + s1 + ") == true");

    bResult = Objects.deepEquals(s1, s2);
    harness.debug("Objects.deepEquals(" + s1 + "," + s2 + ") = " + bResult);
    harness.check(bResult, true, "short array !=; deepEquals(" + s1 + "," + s2 + ") == true");

    harness.debug("nested1(" + nested1 + ") = " + Arrays.toString(nested1));
    harness.debug("nested2(" + nested2 + ") = " + Arrays.toString(nested2));

    bResult = Objects.deepEquals(nested1, nested1);
    harness.debug("Objects.deepEquals(" + nested1 + "," + nested1 + ") = " + bResult);
    harness.check(bResult, true, "nested array ==; deepEquals(" + nested1 + "," + nested1 + ") == true");

    bResult = Objects.deepEquals(nested1, nested2);
    harness.debug("Objects.deepEquals(" + nested1 + "," + nested2 + ") = " + bResult);
    harness.check(bResult, true, "nested array !=; deepEquals(" + nested1 + "," + nested2 + ") == true");

    bResult = Objects.deepEquals(veg1, i1);
    harness.debug("Objects.deepEquals(" + veg1 + "," + i1 + ") = " + bResult);
    harness.check(bResult, false, "heterogenous arrays; deepEquals(" + veg1 + "," + i1 + ") == false");

    bResult = Objects.deepEquals(i1, veg1);
    harness.debug("Objects.deepEquals(" + i1 + "," + veg1 + ") = " + bResult);
    harness.check(bResult, false, "heterogenous arrays; deepEquals(" + i1 + "," + veg1 + ") == false");
  }
}
