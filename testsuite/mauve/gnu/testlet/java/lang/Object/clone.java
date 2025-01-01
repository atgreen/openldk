/* Copyright (C) 2001 Eric Blake

   This file is part of Mauve.

   Mauve is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Mauve is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Mauve; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.
*/

// Tags: JDK1.0

package gnu.testlet.java.lang.Object;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

/**
 * This class tests that the default Object.clone does not invoke a
 * constructor, and that it creates a new object of the same class.
 * Classes that override clone(), however, are permitted to change this
 * behavior.
 *
 * This class also tests that array.clone behaves correctly.
 */
public final class clone implements Testlet, Cloneable
{
  private static int count = 0;
  private int prim = 42;
  private Object obj = this;
  private Testlet test = this;
  private float[] array = {};

  public clone()
  {
    count++;
  }

  public void test (TestHarness harness)
  {
    int my_count = count;
    clone copy = null;
    try
      {
	copy = (clone) clone();
      }
    catch (CloneNotSupportedException cnse)
      {
	harness.fail("clone should pass on Cloneable object");
      }
    harness.check(copy != this, "clone built distinct object");
    harness.check(copy instanceof clone, "clone built same class - 1");
    harness.check(copy.getClass() == clone.class, "clone built same class - 2");
    harness.check(count == my_count, "clone called no constructor");
    harness.check(copy.prim == 42, "primitive field cloned correctly");
    harness.check(copy.obj == this, "object field cloned correctly");
    harness.check(copy.test == this, "interface field cloned correctly");
    harness.check(copy.array == array, "array field cloned correctly");

    int[] iarray = { 1, 2 };
    int[] icopy = (int[]) iarray.clone();
    Object[] oarray = { new Object() };
    Object[] ocopy = (Object[]) oarray.clone();

    harness.check(iarray != icopy, "cloned arrays are distinct - 1");
    harness.check(oarray != ocopy, "cloned arrays are distinct - 2");
    harness.check(iarray.length == icopy.length, "cloned arrays have same length - 1");
    harness.check(oarray.length == ocopy.length, "cloned arrays have same length - 2");

    harness.check(iarray.getClass() == icopy.getClass(), "cloned arrays have same type - 1");
    harness.check(oarray.getClass() == ocopy.getClass(), "cloned arrays have same type - 2");

    harness.check(iarray.getClass().getComponentType() == icopy.getClass().getComponentType(), "cloned arrays have same component type - 1");
    harness.check(oarray.getClass().getComponentType() == ocopy.getClass().getComponentType(), "cloned arrays have same component type - 2");

    harness.check(iarray[0] == icopy[0], "cloned contents are same - 1");
    harness.check(iarray[1] == icopy[1], "cloned contents are same - 2");
    harness.check(oarray[0] == ocopy[0], "cloned contents are same - 3");
  }
}
