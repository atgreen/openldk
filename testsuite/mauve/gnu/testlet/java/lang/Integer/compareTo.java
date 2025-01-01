/* Copyright (C) 2001 Eric Blake <ebb9@email.byu.edu>
 *
 * This file is part of Mauve.
 *
 * Mauve is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * Mauve is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Mauve; see the file COPYING.  If not, write to
 * the Free Software Foundation, 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.  */

// Tags: JDK1.2

package gnu.testlet.java.lang.Integer;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

/**
 * This class tests the compareTo methods of Integer.  It is separate
 * from other classes in the package because the methods did not exist
 * before JDK 1.2.
 */
public class compareTo implements Testlet
{
  public static final int LESS = -1;
  public static final int EQUAL = 0;
  public static final int GREATER = 1;
  TestHarness harness;
  void compare(Integer i1, Integer i2, int expected)
  {
    // the result need not be -1, 0, 1; just <0, 0, >0
    int result = i1.compareTo(i2);
    switch (expected)
      {
      case LESS:
	harness.check(result < 0);
	break;
      case EQUAL:
	harness.check(result == 0);
	break;
      case GREATER:
	harness.check(result > 0);
	break;
      default:
	throw new Error();
      }
  }
  public void test(TestHarness harness)
  {
    this.harness = harness;
    Integer min = new Integer(Integer.MIN_VALUE);
    Integer negone = new Integer(-1);
    Integer zero = new Integer(0);
    Integer posone = new Integer(1);
    Integer max = new Integer(Integer.MAX_VALUE);

    harness.checkPoint("compareTo");
    compare(min, min, EQUAL);
    compare(min, negone, LESS);
    compare(min, zero, LESS);
    compare(min, posone, LESS);
    compare(min, max, LESS);

    compare(negone, min, GREATER);
    compare(negone, negone, EQUAL);
    compare(negone, zero, LESS);
    compare(negone, posone, LESS);
    compare(negone, max, LESS);

    compare(zero, min, GREATER);
    compare(zero, negone, GREATER);
    compare(zero, zero, EQUAL);
    compare(zero, posone, LESS);
    compare(zero, max, LESS);

    compare(posone, min, GREATER);
    compare(posone, negone, GREATER);
    compare(posone, zero, GREATER);
    compare(posone, posone, EQUAL);
    compare(posone, max, LESS);

    compare(max, min, GREATER);
    compare(max, negone, GREATER);
    compare(max, zero, GREATER);
    compare(max, posone, GREATER);
    compare(max, max, EQUAL);

    Object o = zero;
    boolean ok;
    harness.check(((Comparable)zero).compareTo(o) == 0);

    ok = false;
    try
      {
	zero.compareTo((Integer) null);
      }
    catch (NullPointerException e)
      {
	ok = true;
      }
    harness.check(ok);

    ok = false;
    try
      {
	((Comparable)zero).compareTo((Object) null);
      }
    catch (NullPointerException e)
      {
	ok = true;
      }
    harness.check(ok);

    ok = false;
    try
      {
	((Comparable)zero).compareTo(new Object());
      }
    catch (ClassCastException e)
      {
	ok = true;
      }
    harness.check(ok);
  }
}
