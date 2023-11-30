// Tags: JDK1.1

// Copyright (C) 2002 Free Software Foundation, Inc.

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

package gnu.testlet.java.lang.reflect.Method;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.lang.reflect.Method;

/**
 * Tests java.lang.reflect.Method.equals().
 *
 * @author Mark Wielaard <mark@klomp.org>
 */
public class equals implements Testlet
{
  void q() { }
  void r() { }

  public String toString() { return "funny equals class thing"; }

  public void m() { }
  public void m(String s) { }

  private Method getMethod(Class c, String n, Class[] ts)
  {
    try
      {
        return c.getDeclaredMethod(n, ts);
      }
    catch (NoSuchMethodException nsme)
      {
        throw new RuntimeException("Warning - no such method: " + c + ", " + n);
      }
  }
      
  public void test (TestHarness harness)
  {
    Method m1, m2;
    Class[] ts;

    ts = new Class[0];
    m1 = getMethod(equals.class, "q", ts);
    m2 = getMethod(equals.class, "q", ts);
    harness.check(m1.equals(m2), "same method q");

    m2 = getMethod(equals.class, "r", ts);
    harness.check(!m1.equals(m2), "different method names q and r");

    m1 = getMethod(String.class, "toString", ts);
    m2 = getMethod(equals.class, "toString", ts);
    harness.check(!m1.equals(m2), "different declaring classes for toString");

    m1 = getMethod(equals.class, "m", ts);
    ts = new Class[1];
    ts[0] = String.class;
    m2 = getMethod(equals.class, "m", ts);
    harness.check(!m1.equals(m2), "different argument types m");

    harness.check(!m1.equals(null), "nothing equals null");
  }
}
