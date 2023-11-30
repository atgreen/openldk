/* Simple.java -- Simple SetOfIntegerSyntax tests
   Copyright (C) 2005 Red Hat, Inc.
This file is part of Mauve.

Mauve is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Mauve is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Mauve; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

*/

// Tags: JDK1.4


package gnu.testlet.javax.print.attribute.SetOfIntegerSyntax;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.print.attribute.SetOfIntegerSyntax;

public class Simple extends SetOfIntegerSyntax implements Testlet
{

  // Constructor for the test harness.
  public Simple()
  {
    super(3);
  }

  public Simple(int lowerBound, int upperBound)
  {
    super(lowerBound, upperBound);
  }

  public Simple(int member)
  {
    super(member);
  }

  public Simple(int[][] members)
  {
    super(members);
  }

  public Simple(String s)
  {
    super(s);
  }

  public void test(TestHarness harness)
  {
    SetOfIntegerSyntax single = new Simple(5);
    SetOfIntegerSyntax range = new Simple(new int[][] { { 1, 5 } });
    SetOfIntegerSyntax rangeTwo = new Simple(
        new int[][] { { 1, 5 }, { 10, 12 } });

    harness.checkPoint("single-value equals");
    harness.check(new Simple(5), single);
    harness.check(new Simple(new int[][] { { 5 } }), single);
    harness.check(new Simple(new int[][] { { 1, 0 }, { 5, 5 } }), single);
    harness.check(new Simple("5"), single);
    harness.check(new Simple("1-0,5"), single);
    harness.check(new Simple("5,1:0,5"), single);

    harness.checkPoint("single-range equals");
    harness.check(new Simple(new int[][] { { 1, 5 } }), range);
    harness.check(new Simple("1-5"), range);
    harness.check(new Simple("1:5,5-1"), range);
    harness.check(new Simple("1-3,1-5"), range);
    harness.check(new Simple("1-5,1-3"), range);
    harness.check(new Simple("1-3,2-5"), range);
    harness.check(new Simple("1-3,4-5"), range);
    harness.check(new Simple("4-5,1-3"), range);
    harness.check(new Simple(1, 5), range);

    harness.checkPoint("two-range equals");
    harness.check(new Simple(new int[][] { { 10, 12 }, { 1, 5 } }), rangeTwo);
    harness.check(new Simple("1-3,2-5,10,11:12"), rangeTwo);

    harness.checkPoint("next");
    harness.check(single.next(0), 5);
    harness.check(single.next(5), -1);
    harness.check(single.next(38), -1);
    harness.check(range.next(0), 1);
    harness.check(range.next(1), 2);
    harness.check(range.next(4), 5);
    harness.check(range.next(5), -1);
    harness.check(rangeTwo.next(5), 10);

    harness.checkPoint("hashCode");
    harness.check(single.hashCode(), 10);
    harness.check(range.hashCode(), 6);

    harness.checkPoint("toString");
    harness.check(single.toString(), "5");
    harness.check(range.toString(), "1-5");
    harness.check(rangeTwo.toString(), "1-5,10-12");

    harness.checkPoint("contains");
    harness.check(single.contains(5));
    harness.check(range.contains(2));
    harness.check(rangeTwo.contains(12));
    harness.check(single.contains(6), false);
    harness.check(range.contains(6), false);
    harness.check(rangeTwo.contains(8), false);

    harness.checkPoint("constructors");
    try
      {
        new Simple((String) null);
        harness.check(true);
      }
    catch (NullPointerException e)
      {
        harness.check(false);
      }

    try
      {
        new Simple((int[][]) null);
        harness.check(true);
      }
    catch (NullPointerException e)
      {
        harness.check(false);
      }

    try
      {
        new Simple(new int[][] { { 1, 5 }, null });
        harness.check(false);
      }
    catch (NullPointerException e)
      {
        harness.check(true);
      }

    try
      {
        new Simple(new int[][] { null });
        harness.check(false);
      }
    catch (NullPointerException e)
      {
        harness.check(true);
      }

    try
      {
        new Simple(new int[][] { { 1, 2, 3 } });
        harness.check(false);
      }
    catch (IllegalArgumentException e)
      {
        harness.check(true);
      }

    try
      {
        new Simple(new int[][] { { -1, 2 } });
        harness.check(false);
      }
    catch (IllegalArgumentException e)
      {
        harness.check(true);
      }

    try
      {
        new Simple(14, 12);
        harness.check(true);
      }
    catch (NullPointerException e)
      {
        harness.check(false);
      }

    harness.checkPoint("normalization");
    harness.check(new Simple("17- 99,1:3,12").toString(), "1-3,12,17-99");
    harness.check(new Simple("17- 99,19-20,14-18").toString(), "14-99");

  }
}
