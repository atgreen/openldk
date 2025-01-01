/* Copyright (C) 2010 Red Hat, Inc.

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

// Tags: JDK1.5

package gnu.testlet.java.lang.Class;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.lang.reflect.Array;

public class IsAnonymousClass implements Testlet
{
  public void test(TestHarness harness)
  {
    Class<?> anon = (new Object() {
        public String toString() { return "Hello!"; }
      }).getClass();
    Class<?> anonArray = Array.newInstance(anon, 1).getClass();

    harness.check(int.class.isAnonymousClass(), false,
                  "Primitive type class");
    harness.check(int[].class.isAnonymousClass(), false,
                  "Primitive type one-dimensional array class");
    harness.check(int[][].class.isAnonymousClass(), false,
                  "Primitive type multi-dimensional array class");
    harness.check(Object[].class.isAnonymousClass(), false,
                  "Object type one-dimensional array class");
    harness.check(Object.class.isAnonymousClass(), false,
                  "Object type class");
    harness.check(InnerClass.class.isAnonymousClass(), false,
                  "Inner class");
    harness.check(anon.isAnonymousClass(), true, "Anonymous class");
    harness.check(anonArray.isAnonymousClass(), false, "Anonymous array class");
  }

  public static class InnerClass { };

}

